{-# LANGUAGE OverloadedStrings #-}

module UI.UI (uiMain) where

import Brick
  ( App (..),
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
import Brick.Widgets.Edit
  ( editor,
    getEditContents,
    renderEditor,
  )
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import Config (getConfig, getInitialTimer)
import qualified Config as CFG (createConfigFileIfNotExists, updateConfig, writeConfig)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((+=), (.=), (^.))
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get),
    forever,
    void,
    when,
  )
import qualified Data.Text as Txt
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as VE
import Resources
  ( AppState (..),
    Name (..),
    Task,
    TaskAction (Edit, Insert),
    TaskListOperation (..),
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
    timerRunning,
  )
import qualified Resources as R
import Task (getTasks, mkTask, taskExists, updateTaskList, writeTasks, createTasksFileIfNotExists)
import Timer (tickTimer)
import UI.Attributes
  ( attributes,
    selectedTaskAttr,
    taskCompletedLabelAttr,
    taskCompletedWhiteBgLabelAttr,
    taskPendingLabelAttr,
    taskPendingWhiteBgLabelAttr,
  )
import UI.Config (drawConfig)
import UI.Timer (drawTimers)
import Data.Time (getCurrentTime, UTCTime (UTCTime))

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
  createTasksFileIfNotExists
  tasks <- getTasks
  setPomodoroInitialTimer <- getInitialTimer R.Pomodoro
  setShortBreakInitialTimer <- getInitialTimer R.ShortBreak
  setLongBreakInitialTimer <- getInitialTimer R.LongBreak
  let appState = createAppStateWithTasks setPomodoroInitialTimer setShortBreakInitialTimer setLongBreakInitialTimer tasks
  return appState

createAppStateWithTasks :: Int -> Int -> Int -> [Task] -> AppState
createAppStateWithTasks
  pomodoroTimerDuration
  shortBreakTimerDuration
  longBreakTimerDuration
  tasks =
     let taskListWidget = BL.list (TaskList Pomodoro R.Active) (DV.fromList tasks) 1
     in AppState
          { _timerRunning = False,
            _pomodoroCounter = 0,
            _pomodoroTimer = pomodoroTimerDuration,
            _shortBreakTimer = shortBreakTimerDuration,
            _longBreakTimer = longBreakTimerDuration,
            _taskEditor = editor (TaskEdit Insert) (Just 5) "",
            _focus = BF.focusRing [TaskList Pomodoro R.Active, TaskList ShortBreak R.Active, TaskList LongBreak R.Active, TaskEdit Insert, TaskEdit Edit, Commands, Config],
            _taskList = taskListWidget
          }

app :: App AppState Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const attributes
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
              tasksFromFile <- liftIO getTasks
              if not (taskExists tasksFromFile insertedContent) && not (Txt.null insertedContent)
                then do
                  task <- liftIO $ mkTask insertedContent $ Just False
                  taskList .= BL.listInsert (length $ s ^. taskList) task (s ^. taskList)
                  _ <- liftIO $ writeTasks updateTaskList (AppendTask task)
                  taskEditor .= editor (TaskEdit Insert) (Just 5) ""
                  focus .= BF.focusSetCurrent (TaskList Pomodoro R.Active) (s ^. focus)
                else do
                  taskEditor .= editor (TaskEdit Insert) (Just 5) ""
                  focus .= BF.focusSetCurrent (TaskList Pomodoro R.Active) (s ^. focus)
            (V.KEsc, []) -> do
              focus .= BF.focusSetCurrent (TaskList Pomodoro R.Active) (s ^. focus)
            _ -> do
              BT.zoom taskEditor $ BE.handleEditorEvent ev
        Just (TaskEdit Edit) ->
          case (k, ms) of
            (V.KIns, []) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (_, selectedTask) -> do
                  let editedContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
                  tasksFromFile <- liftIO getTasks
                  when (not (taskExists tasksFromFile editedContent) && not (Txt.null editedContent)) $ do
                    modifiedTaskList <- liftIO $ writeTasks updateTaskList (EditTask selectedTask editedContent)
                    taskList .= BL.listReplace (DV.fromList modifiedTaskList) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                    taskEditor .= editor (TaskEdit Edit) (Just 5) ""
                    focus .= BF.focusSetCurrent (TaskList Pomodoro R.Active) (s ^. focus)
                Nothing -> return ()
            (V.KEsc, []) -> do
              taskEditor .= editor (TaskEdit Insert) (Just 5) ""
              focus .= BF.focusSetCurrent (TaskList Pomodoro R.Active) (s ^. focus)
            _ -> do
              BT.zoom taskEditor $ BE.handleEditorEvent ev
        Just cfs@(TaskList _ _) ->
          case (k, ms) of
            (V.KChar 'q', []) -> do
              halt
            (V.KChar 'c', []) -> do
              focus .= BF.focusSetCurrent Commands (s ^. focus)
            (V.KChar 's', []) -> do
              timerRunning .= not (s ^. timerRunning)
            (V.KChar 'r', []) -> do
              case cfs of
                TaskList Pomodoro _ -> do
                  initialTimer <- liftIO $ getInitialTimer R.Pomodoro
                  pomodoroTimer .= initialTimer
                TaskList ShortBreak _ -> do
                  initialTimer <- liftIO $ getInitialTimer R.ShortBreak
                  shortBreakTimer .= initialTimer
                TaskList LongBreak _ -> do
                  initialTimer <- liftIO $ getInitialTimer R.LongBreak
                  longBreakTimer .= initialTimer
            (V.KChar 'i', []) -> do
              case cfs of
                TaskList Pomodoro _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.Pomodoro (config ^. pomodoroInitialTimer + 60)) config
                  pomodoroTimer += 60
                TaskList ShortBreak _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.ShortBreak (config ^. shortBreakInitialTimer + 60)) config
                  shortBreakTimer += 60
                TaskList LongBreak _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.LongBreak (config ^. longBreakInitialTimer + 60)) config
                  longBreakTimer += 60
            (V.KChar 'd', []) -> do
              case cfs of
                TaskList Pomodoro _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.Pomodoro (max ((config ^. pomodoroInitialTimer) - 60) 1)) config
                  pomodoroTimer .= max ((s ^. pomodoroTimer) - 60) 1
                TaskList ShortBreak _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.ShortBreak (max ((config ^. shortBreakInitialTimer) - 60) 1)) config
                  shortBreakTimer .= max ((s ^. shortBreakTimer) - 60) 1
                TaskList LongBreak _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.LongBreak (max ((config ^. longBreakInitialTimer) - 60) 1)) config
                  longBreakTimer .= max ((s ^. longBreakTimer) - 60) 1
            (V.KChar 'I', []) -> do
              case cfs of
                TaskList Pomodoro _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.Pomodoro (config ^. pomodoroInitialTimer + 10)) config
                  pomodoroTimer += 10
                TaskList ShortBreak _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.ShortBreak (config ^. shortBreakInitialTimer + 10)) config
                  shortBreakTimer += 10
                TaskList LongBreak _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.LongBreak (config ^. longBreakInitialTimer + 10)) config
                  longBreakTimer += 10
            (V.KChar 'D', []) -> do
              case cfs of
                TaskList Pomodoro _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.Pomodoro (max ((config ^. pomodoroInitialTimer) - 10) 1)) config
                  pomodoroTimer .= max ((s ^. pomodoroTimer) - 10) 1
                TaskList ShortBreak _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.ShortBreak (max ((config ^. shortBreakInitialTimer) - 10) 1)) config
                  shortBreakTimer .= max ((s ^. shortBreakTimer) - 10) 1
                TaskList LongBreak _ -> do
                  _ <- liftIO $ forkIO $ do
                    config <- getConfig
                    CFG.writeConfig $
                      CFG.updateConfig (R.UpdateInitialTimer R.LongBreak (max ((config ^. longBreakInitialTimer) - 10) 1)) config
                  longBreakTimer .= max ((s ^. shortBreakTimer) - 10) 1
            (V.KBackTab, []) -> do
              timerRunning .= False
              case cfs of
                TaskList Pomodoro ctg -> focus .= BF.focusSetCurrent (TaskList ShortBreak ctg) (s ^. focus)
                TaskList ShortBreak ctg -> focus .= BF.focusSetCurrent (TaskList LongBreak ctg) (s ^. focus)
                TaskList LongBreak ctg -> focus .= BF.focusSetCurrent (TaskList Pomodoro ctg) (s ^. focus)
            _ -> BT.zoom taskList $ BL.handleListEventVi BL.handleListEvent vev
        Just (TaskList _ R.Active) ->
          case (k, ms) of
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
            (V.KChar 't', []) -> do
              focus .= BF.focusSetCurrent (TaskEdit Insert) (s ^. focus)
            (V.KChar 'a', [V.MCtrl]) -> do
              currentTime <- liftIO getCurrentTime
              let UTCTime today _ = currentTime
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (selectedIndex, selectedTask) -> do
                  modifiedTaskList <- liftIO $ writeTasks updateTaskList (ArchiveTask selectedTask today)
                  taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just selectedIndex) (s ^. taskList)
                Nothing -> return ()
            _ -> BT.zoom taskList $ BL.handleListEventVi BL.handleListEvent vev
        Just Commands ->
          focus .= BF.focusSetCurrent (TaskList Pomodoro R.Active) (s ^. focus)
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
      \t           -> Add a task\n \
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
drawTaskList s = do
  let currentFocus = BF.focusGetCurrent (s ^. focus)
    in B.borderWithLabel (str "Tasks")
      $ BL.renderList drawTaskListItem (BF.focusGetCurrent (s ^. focus) == currentFocus) (s ^. taskList)

drawTaskListItem :: Bool -> Task -> Widget Name
drawTaskListItem sel task
  | sel =
      withAttr selectedTaskAttr $
        padLeft (Pad 1) $
          taskListItem task sel
  | otherwise =
      taskListItem task sel

taskListItem :: Task -> Bool -> Widget Name
taskListItem task sel =
  vBox
    [ withBorderStyle BS.unicodeRounded $
        B.border $
          padLeft (Pad 1) $
            txtWrap (task ^. R.taskContent)
              <=> padTop
                (Pad 1)
                ( emptyWidget
                    <=> drawTaskStatus (task ^. R.taskCompleted) sel
                )
    ]

drawTaskStatus :: Bool -> Bool -> Widget Name
drawTaskStatus isCompleted isSelected
  | isCompleted =
      if isSelected
        then withAttr taskCompletedWhiteBgLabelAttr $ txt "Completed"
        else withAttr taskCompletedLabelAttr $ txt "Completed"
  | otherwise =
      if isSelected
        then withAttr taskPendingWhiteBgLabelAttr $ txt "Pending"
        else withAttr taskPendingLabelAttr $ txt "Pending"
