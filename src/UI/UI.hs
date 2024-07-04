{-# LANGUAGE OverloadedStrings #-}

module UI.UI (uiMain) where

import Brick (
    App (..),
    BrickEvent (AppEvent, VtyEvent),
    EventM,
    Padding (Pad),
    Widget,
    customMain,
    emptyWidget,
    halt,
    padLeft,
    padTop,
    showFirstCursor,
    str,
    strWrap,
    txt,
    txtWrap,
    vBox,
    withAttr,
    withBorderStyle,
    (<=>),
 )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Focus as BF
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Edit (
    editor,
    getEditContents,
    renderEditor,
 )
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((+=), (.=), (^.))
import Control.Monad.State (
    MonadIO (liftIO),
    MonadState (get),
    forever,
    void,
    when,
 )
import Data.Maybe (fromMaybe)
import qualified Data.Text as Txt
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as VE
import Resources (
    AppState (..),
    Name (..),
    TaskAction (Edit, Insert),
    Tick (..),
    Timer (LongBreak, Pomodoro, ShortBreak),
    focus,
    longBreakInitialTimer,
    longBreakTimer,
    pomodoroInitialTimer,
    pomodoroTimer,
    shortBreakInitialTimer,
    shortBreakTimer,
    taskEditor,
    taskList,
    timerRunning, Task, TaskListOperation (..),
 )
import UI.Attributes (
    attributes,
    selectedTaskAttr,
    taskCompletedLabelAttr,
    taskPendingLabelAttr,
 )
import qualified Config as CFG (createConfigFileIfNotExists, updateConfig, writeConfig) 
import UI.Config (drawConfig)
import Timer (tickTimer)
import Config (getInitialTimer, getConfig)
import qualified Resources as R
import Task (getTasks, writeTasks, updateTaskList, mkTask, taskExists)
import UI.Timer (drawTimers)

uiMain :: IO ()
uiMain = do
    eventChan <- newBChan 10
    CFG.createConfigFileIfNotExists
    _ <- forkIO $ forever $ do
        writeBChan eventChan Tick
        threadDelay 1000000
    initialState <- createAppState
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just eventChan) app initialState

createAppState :: IO AppState
createAppState = do
    tasks <- getTasks
    setPomodoroInitialTimer <- getInitialTimer R.Pomodoro
    setShortBreakInitialTimer <- getInitialTimer R.ShortBreak
    setLongBreakInitialTimer <- getInitialTimer R.LongBreak
    let appState = createAppStateWithTasks setPomodoroInitialTimer setShortBreakInitialTimer setLongBreakInitialTimer tasks
    return appState

createAppStateWithTasks :: Int -> Int -> Int -> Maybe [Task] -> AppState
createAppStateWithTasks
    pomodoroTimerDuration
    shortBreakTimerDuration
    longBreakTimerDuration
    maybeTasks =
        let tasksList = fromMaybe [] maybeTasks
            taskListWidget = BL.list (TaskList Pomodoro) (DV.fromList tasksList) 1
         in AppState
                { _timerRunning = False
                , _pomodoroCycleCounter = 0
                , _pomodoroTimer = pomodoroTimerDuration
                , _shortBreakTimer = shortBreakTimerDuration
                , _longBreakTimer = longBreakTimerDuration
                , _taskEditor = editor (TaskEdit Insert) (Just 5) ""
                , _focus = BF.focusRing [TaskList Pomodoro, TaskList ShortBreak, TaskList LongBreak, TaskEdit Insert, TaskEdit Edit, Commands, Config]
                , _taskList = taskListWidget
                }

app :: App AppState Tick Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const attributes
        }

handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent ev =
    case ev of
        (AppEvent Tick) -> do
            s <- get
            tickTimer s
        (VtyEvent vev@(VE.EvKey k ms)) -> do
            s <- get
            case BF.focusGetCurrent $ s ^. focus of
                Just (TaskEdit Insert) ->
                    case (k, ms) of
                        (V.KIns, []) -> do
                            let insertedContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
                            taskAlreadyExists <- liftIO $ taskExists insertedContent
                            if not taskAlreadyExists && not (Txt.null insertedContent)
                                then do
                                    let task = mkTask insertedContent $ Just False
                                    taskList .= BL.listInsert (length $ s ^. taskList) task (s ^. taskList)
                                    _ <- liftIO $ writeTasks updateTaskList (AppendTask task)
                                    taskEditor .= editor (TaskEdit Insert) (Just 5) ""
                                    focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                                else do
                                    taskEditor .= editor (TaskEdit Insert) (Just 5) ""
                                    focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                        (V.KEsc, []) -> do
                            focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                        _ -> do
                            BT.zoom taskEditor $ BE.handleEditorEvent ev
                Just (TaskEdit Edit) ->
                    case (k, ms) of
                        (V.KIns, []) -> do
                            let selectedListTask = BL.listSelectedElement (s ^. taskList)
                            case selectedListTask of
                                Just (_, selectedTask) -> do
                                    let editedContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
                                    taskAlreadyExists <- liftIO $ taskExists editedContent
                                    when (not taskAlreadyExists && not (Txt.null editedContent)) $ do
                                        modifiedTaskList <- liftIO $ writeTasks updateTaskList (EditTask selectedTask editedContent)
                                        taskList .= BL.listReplace (DV.fromList modifiedTaskList) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                                        taskEditor .= editor (TaskEdit Edit) (Just 5) ""
                                        focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                                Nothing -> return ()
                        (V.KEsc, []) -> do
                            taskEditor .= editor (TaskEdit Insert) (Just 5) ""
                            focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                        _ -> do
                            BT.zoom taskEditor $ BE.handleEditorEvent ev
                Just cfs@(TaskList _) ->
                    case (k, ms) of
                        (V.KChar 'q', []) -> do
                            halt
                        (V.KChar 'c', []) -> do
                            focus .= BF.focusSetCurrent Commands (s ^. focus)
                        (V.KChar 's', []) -> do
                            timerRunning .= not (s ^. timerRunning)
                        (V.KChar 'r', []) -> do
                            case cfs of
                                TaskList Pomodoro -> do
                                    initialTimer <- liftIO $ getInitialTimer R.Pomodoro
                                    pomodoroTimer .= initialTimer
                                TaskList ShortBreak -> do
                                    initialTimer <- liftIO $ getInitialTimer R.ShortBreak
                                    shortBreakTimer .= initialTimer
                                TaskList LongBreak -> do
                                    initialTimer <- liftIO $ getInitialTimer R.LongBreak
                                    longBreakTimer .= initialTimer
                        (V.KChar 'i', []) -> do
                            case cfs of
                                TaskList Pomodoro -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.Pomodoro (config ^. pomodoroInitialTimer + 60)) config
                                    pomodoroTimer += 60
                                TaskList ShortBreak -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.ShortBreak (config ^. shortBreakInitialTimer + 60)) config
                                    shortBreakTimer += 60
                                TaskList LongBreak -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.LongBreak (config ^. longBreakInitialTimer + 60)) config
                                    longBreakTimer += 60
                        (V.KChar 'd', []) -> do
                            case cfs of
                                TaskList Pomodoro -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.Pomodoro (max ((config ^. pomodoroInitialTimer) - 60) 1)) config
                                    pomodoroTimer .= max ((s ^. pomodoroTimer) - 60) 1
                                TaskList ShortBreak -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.ShortBreak (max ((config ^. shortBreakInitialTimer) - 60) 1)) config
                                    shortBreakTimer .= max ((s ^. shortBreakTimer) - 60) 1
                                TaskList LongBreak -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.LongBreak (max ((config ^. longBreakInitialTimer) - 60) 1)) config
                                    longBreakTimer .= max ((s ^. longBreakTimer) - 60) 1
                        (V.KChar 'I', []) -> do
                            case cfs of
                                TaskList Pomodoro -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.Pomodoro (config ^. pomodoroInitialTimer + 10)) config
                                    pomodoroTimer += 10
                                TaskList ShortBreak -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.ShortBreak (config ^. shortBreakInitialTimer + 10)) config
                                    shortBreakTimer += 10
                                TaskList LongBreak -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.LongBreak (config ^. longBreakInitialTimer + 10)) config
                                    longBreakTimer += 10
                        (V.KChar 'D', []) -> do
                            case cfs of
                                TaskList Pomodoro -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.Pomodoro (max ((config ^. pomodoroInitialTimer) - 10) 1)) config
                                    pomodoroTimer .= max ((s ^. pomodoroTimer) - 10) 1
                                TaskList ShortBreak -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.ShortBreak (max ((config ^. shortBreakInitialTimer) - 10) 1)) config
                                    shortBreakTimer .= max ((s ^. shortBreakTimer) - 10) 1
                                TaskList LongBreak -> do
                                    _ <- liftIO $ forkIO $ do
                                        config <- getConfig
                                        CFG.writeConfig $
                                            CFG.updateConfig (R.UpdateInitialTimer R.LongBreak (max ((config ^. longBreakInitialTimer) - 10) 1)) config
                                    longBreakTimer .= max ((s ^. longBreakTimer) - 10) 1
                        (V.KBackTab, []) -> do
                            timerRunning .= False
                            case cfs of
                                TaskList Pomodoro -> focus .= BF.focusSetCurrent (TaskList ShortBreak) (s ^. focus)
                                TaskList ShortBreak -> focus .= BF.focusSetCurrent (TaskList LongBreak) (s ^. focus)
                                TaskList LongBreak -> focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                        (V.KChar 'e', []) -> do
                            let selectedListTask = BL.listSelectedElement (s ^. taskList)
                            case selectedListTask of
                                Just (_, selectedTask) -> do
                                    let selectedTaskContent = selectedTask ^. R.taskContent
                                    taskEditor .= editor (TaskEdit Edit) (Just 5) selectedTaskContent
                                    focus .= BF.focusSetCurrent (TaskEdit Edit) (s ^. focus)
                                Nothing -> return ()
                        (V.KChar 'c', [V.MCtrl]) -> do
                            let selectedListTask = BL.listSelectedElement (s ^. taskList)
                            case selectedListTask of
                                Just (_, selectedTask) -> do
                                    modifiedTaskList <- liftIO $ writeTasks updateTaskList (ChangeTaskCompletion selectedTask)
                                    taskList .= BL.listReplace (DV.fromList modifiedTaskList) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                                Nothing -> return ()
                        (V.KChar 'p', []) -> do
                            focus .= BF.focusSetCurrent Config (s ^. focus)
                        (V.KDel, []) -> do
                            let selectedListTask = BL.listSelectedElement (s ^. taskList)
                            case selectedListTask of
                                Just (selectedIndex, selectedTask) -> do
                                    modifiedTaskList <- liftIO $ writeTasks updateTaskList (DeleteTask selectedTask)
                                    if selectedIndex - 1 == length modifiedTaskList
                                        then taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just selectedIndex) (s ^. taskList)
                                        else
                                            if selectedIndex == 0
                                                then taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just 0) (s ^. taskList)
                                                else taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just $ length modifiedTaskList - 1) (s ^. taskList)
                                Nothing -> return ()
                        (V.KIns, []) -> do
                            focus .= BF.focusSetCurrent (TaskEdit Insert) (s ^. focus)
                        _ -> BT.zoom taskList $ BL.handleListEventVi BL.handleListEvent vev
                Just Commands ->
                    focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                _ -> return ()
        _ -> return ()

drawUI :: AppState -> [Widget Name]
drawUI s =
    case BF.focusGetCurrent (s ^. focus) of
        fcs@(Just (TaskEdit _)) -> [B.border $ C.center $ drawTimers s <=> drawTaskList s <=> drawTaskEditor s <=> drawCommands fcs]
        Just Commands -> [B.border $ C.center drawCommandsScreen]
        Just Config -> [drawConfig]
        _ -> [B.border $ C.center $ drawTimers s <=> drawTaskList s <=> drawCommandsTip]

drawCommandsTip :: Widget Name
drawCommandsTip = C.hCenter $ str "press c to see the commands"

drawCommandsScreen :: Widget Name
drawCommandsScreen =
    C.hCenter $
        str
            " q           -> Quit application \n \
            \Shift + Tab -> Go to next timer \n \
            \s           -> Start/Stop timer\n \
            \r           -> Reset timer\n \
            \i/d         -> Increase/Decrease timer by 1min\n \
            \I/D         -> Increased/Decrease timer by 10sec\n \
            \Insert      -> Add a task\n \
            \e           -> Edit a task\n \
            \Del         -> Delete a task\n \
            \Ctrl + C    -> Change a task's status\n"

drawCommands :: Maybe Name -> Widget Name
drawCommands currentFocus = do
    case currentFocus of
        Just (TaskEdit Insert) -> strWrap "ESC: Cancel task creation  Ins: Create task  "
        Just (TaskEdit Edit) -> strWrap "ESC: Cancel task edition  Ins: Save task  "
        _ -> emptyWidget

drawTaskEditor :: AppState -> Widget Name
drawTaskEditor s =
    padTop (Pad 1) $
        C.hCenter $
            B.borderWithLabel (str "Task") $
                renderEditor drawTaskEditorContent (BF.focusGetCurrent (s ^. focus) == Just (TaskEdit Insert) || BF.focusGetCurrent (s ^. focus) == Just (TaskEdit Edit)) (s ^. taskEditor)

drawTaskEditorContent :: [Txt.Text] -> Widget Name
drawTaskEditorContent t = txt (Txt.unlines t)

drawTaskList :: AppState -> Widget Name
drawTaskList s =
    do
        B.borderWithLabel (str "Tasks")
        $ BL.renderList drawTaskListItem (BF.focusGetCurrent (s ^. focus) == Just (TaskList Pomodoro)) (s ^. taskList)

drawTaskListItem :: Bool -> Task -> Widget Name
drawTaskListItem sel task
    | sel =
        withAttr selectedTaskAttr $
            padLeft (Pad 1) $
                taskListItem task
    | otherwise =
        taskListItem task

taskListItem :: Task -> Widget Name
taskListItem task =
    vBox
        [ withBorderStyle BS.unicodeRounded $
            B.border $
                padLeft (Pad 1) $
                    txtWrap (task ^. R.taskContent)
                        <=> if task ^. R.taskCompleted
                            then
                                withAttr taskCompletedLabelAttr $
                                    padTop (Pad 1) $
                                        txt "Completed"
                            else
                                withAttr taskPendingLabelAttr $
                                    padTop (Pad 1) $
                                        txt "Pending"
        ]