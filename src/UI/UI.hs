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
    padLeftRight,
    padTop,
    padTopBottom,
    showFirstCursor,
    str,
    strWrap,
    txt,
    txtWrap,
    vBox,
    withAttr,
    withBorderStyle,
    (<+>),
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
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens (uses, (+=), (.=), (^.))
import Control.Monad.State
  ( MonadIO (liftIO),
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
import qualified Notify as NT
import Resources
  ( AppState (..),
    Name (..),
    TaskAction (Edit, Insert),
    Tick (..),
    Timer (LongBreak, Pomodoro, ShortBreak),
    focus,
    longBreakInitialTimer,
    longBreakTimer,
    pomodoroCycleCounter,
    pomodoroInitialTimer,
    pomodoroTimer,
    shortBreakInitialTimer,
    shortBreakTimer,
    taskEditor,
    taskList,
    timerRunning,
  )
import qualified Task.File as TKF
import qualified Task.Task as TK
import Text.Printf (printf)
import UI.Attributes
  ( attributes,
    selectedTaskAttr,
    selectedTimerAttr,
    timerAttr,
  )

uiMain :: IO ()
uiMain = do
  eventChan <- newBChan 10
  tasksFilePath <- TKF.getTasksFilePath
  TKF.createTasksFileIfNotExists tasksFilePath
  _ <- forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 1000000
  initialState <- createAppState 1500 300 900
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just eventChan) app initialState

createAppState :: Int -> Int -> Int -> IO AppState
createAppState pomodoroTimerDuration shortBreakTimerDuration longBreakTimerDuration = do
  tasks <- TKF.getTasks
  let appState = createAppStateWithTasks pomodoroTimerDuration shortBreakTimerDuration longBreakTimerDuration tasks
  return appState

createAppStateWithTasks :: Int -> Int -> Int -> Maybe [TK.Task] -> AppState
createAppStateWithTasks
  pomodoroTimerDuration
  shortBreakTimerDuration
  longBreakTimerDuration
  maybeTasks =
    let tasksList = fromMaybe [] maybeTasks
        taskListWidget = BL.list (TaskList Pomodoro) (DV.fromList tasksList) 1
     in AppState
          { _timerRunning = False,
            _pomodoroCycleCounter = 0,
            _pomodoroInitialTimer = pomodoroTimerDuration,
            _pomodoroTimer = pomodoroTimerDuration,
            _shortBreakInitialTimer = shortBreakTimerDuration,
            _shortBreakTimer = shortBreakTimerDuration,
            _longBreakInitialTimer = longBreakTimerDuration,
            _longBreakTimer = longBreakTimerDuration,
            _taskEditor = editor (TaskEdit Insert) (Just 5) "",
            _focus = BF.focusRing [TaskList Pomodoro, TaskList ShortBreak, TaskList LongBreak, TaskEdit Insert, TaskEdit Edit, Commands],
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
              taskAlreadyExists <- liftIO $ TKF.taskExists insertedContent
              if not taskAlreadyExists && not (Txt.null insertedContent)
                then do
                  let task = TK.mkTask insertedContent $ Just False
                  taskList .= BL.listInsert (length $ s ^. taskList) task (s ^. taskList)
                  _ <- liftIO $ TKF.writeTasks TK.updateTaskList (TK.AppendTask task)
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
                  taskAlreadyExists <- liftIO $ TKF.taskExists editedContent
                  when (not taskAlreadyExists && not (Txt.null editedContent)) $ do
                    modifiedTaskList <- liftIO $ TKF.writeTasks TK.updateTaskList (TK.EditTask selectedTask editedContent)
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
                TaskList Pomodoro -> pomodoroTimer .= s ^. pomodoroInitialTimer
                TaskList ShortBreak -> shortBreakTimer .= s ^. shortBreakInitialTimer
                TaskList LongBreak -> longBreakTimer .= s ^. longBreakInitialTimer
            (V.KChar 'i', []) -> do
              case cfs of
                TaskList Pomodoro -> do
                  pomodoroInitialTimer += 60
                  pomodoroTimer += 60
                TaskList ShortBreak -> do
                  shortBreakInitialTimer += 60
                  shortBreakTimer += 60
                TaskList LongBreak -> do
                  longBreakInitialTimer += 60
                  longBreakTimer += 60
            (V.KChar 'd', []) -> do
              case cfs of
                TaskList Pomodoro -> do
                  pomodoroInitialTimer .= max ((s ^. pomodoroInitialTimer) - 60) 1
                  pomodoroTimer .= max ((s ^. pomodoroTimer) - 60) 1
                TaskList ShortBreak -> do
                  shortBreakInitialTimer .= max ((s ^. shortBreakInitialTimer) - 60) 1
                  shortBreakTimer .= max ((s ^. shortBreakTimer) - 60) 1
                TaskList LongBreak -> do
                  longBreakInitialTimer .= max ((s ^. longBreakInitialTimer) - 60) 1
                  longBreakTimer .= max ((s ^. longBreakTimer) - 60) 1
            (V.KChar 'I', []) -> do
              case cfs of
                TaskList Pomodoro -> do
                  pomodoroInitialTimer += 10
                  pomodoroTimer += 10
                TaskList ShortBreak -> do
                  shortBreakInitialTimer += 10
                  shortBreakTimer += 10
                TaskList LongBreak -> do
                  longBreakInitialTimer += 10
                  longBreakTimer += 10
            (V.KChar 'D', []) -> do
              case cfs of
                TaskList Pomodoro -> do
                  pomodoroInitialTimer .= max ((s ^. pomodoroInitialTimer) - 10) 1
                  pomodoroTimer .= max ((s ^. pomodoroTimer) - 10) 1
                TaskList ShortBreak -> do
                  shortBreakInitialTimer .= max ((s ^. shortBreakInitialTimer) - 10) 1
                  shortBreakTimer .= max ((s ^. shortBreakTimer) - 10) 1
                TaskList LongBreak -> do
                  longBreakInitialTimer .= max ((s ^. longBreakInitialTimer) - 10) 1
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
                  let selectedTaskContent = selectedTask ^. TK.taskContent
                  taskEditor .= editor (TaskEdit Edit) (Just 5) selectedTaskContent
                  focus .= BF.focusSetCurrent (TaskEdit Edit) (s ^. focus)
                Nothing -> return ()
            (V.KChar 'c', [V.MCtrl]) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (_, selectedTask) -> do
                  modifiedTaskList <- liftIO $ TKF.writeTasks TK.updateTaskList (TK.ChangeTaskCompletion selectedTask)
                  taskList .= BL.listReplace (DV.fromList modifiedTaskList) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                Nothing -> return ()
            (V.KDel, []) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (selectedIndex, selectedTask) -> do
                  modifiedTaskList <- liftIO $ TKF.writeTasks TK.updateTaskList (TK.DeleteTask selectedTask)
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

stopTimerAndAlert :: String -> EventM Name AppState ()
stopTimerAndAlert msg = do
  timerRunning .= False
  _ <- liftIO $ forkIO NT.playAlertSound
  NT.alertRoundEnded msg

finishShortBreakRound :: AppState -> EventM Name AppState ()
finishShortBreakRound s = do
  pomodoroCycleCounter .= (s ^. pomodoroCycleCounter) + 1
  updatedCycleCounter <- uses pomodoroCycleCounter id
  if updatedCycleCounter == 4
    then do
      focus .= BF.focusSetCurrent (TaskList LongBreak) (s ^. focus)
    else focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)

tickTimer :: AppState -> EventM Name AppState ()
tickTimer s
  | s ^. timerRunning = case BF.focusGetCurrent (s ^. focus) of
      Just (TaskList Pomodoro) -> do
        pomodoroTimer .= max ((s ^. pomodoroTimer) - 1) 0
        when (s ^. pomodoroTimer == 0) $ do
          pomodoroTimer .= s ^. pomodoroInitialTimer
          stopTimerAndAlert "Pomodoro round ended!"
          focus .= BF.focusSetCurrent (TaskList ShortBreak) (s ^. focus)
      Just (TaskList ShortBreak) -> do
        shortBreakTimer .= max ((s ^. shortBreakTimer) - 1) 0
        when (s ^. shortBreakTimer == 0) $ do
          shortBreakTimer .= s ^. shortBreakInitialTimer
          stopTimerAndAlert "Short break ended!"
          finishShortBreakRound s
      Just (TaskList LongBreak) -> do
        longBreakTimer .= max ((s ^. longBreakTimer) - 1) 0
        when (s ^. longBreakTimer == 0) $ do
          longBreakTimer .= s ^. longBreakInitialTimer
          stopTimerAndAlert "Long break ended!"
          focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
          pomodoroCycleCounter .= 0
      _ -> return ()
  | otherwise = return ()

drawUI :: AppState -> [Widget Name]
drawUI s =
  case BF.focusGetCurrent (s ^. focus) of
    fcs@(Just (TaskEdit _)) -> [B.border $ C.center $ drawTimers s <=> drawTaskList s <=> drawTaskEditor s <=> drawCommands fcs]
    Just Commands -> [B.border $ C.center drawCommandsScreen]
    _ -> [B.border $ C.center $ drawTimers s <=> drawTaskList s <=> drawCommandsTip]

label :: String -> Widget Name
label s = B.border $ padLeftRight 1 $ str s

drawTimers :: AppState -> Widget Name
drawTimers s =
  case BF.focusGetCurrent (s ^. focus) of
    Just (TaskList timer) -> case timer of
      Pomodoro ->
        (C.hCenter (withAttr selectedTimerAttr (label "Pomodoro") <+> padLeftRight 2 (label "Short break") <+> label "Long break") <=>) $
          drawTimer (s ^. pomodoroTimer)
            <=> drawCyclesCounter s
      ShortBreak ->
        (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (withAttr selectedTimerAttr $ label "Short break") <+> label "Long break") <=>) $
          drawTimer (s ^. shortBreakTimer)
            <=> drawCyclesCounter s
      LongBreak ->
        (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (label "Short break") <+> withAttr selectedTimerAttr (label "Long break")) <=>) $
          drawTimer (s ^. longBreakTimer)
            <=> drawCyclesCounter s
    _ ->
      (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (label "Short break") <+> label "Long break") <=>) $
        drawTimer (s ^. pomodoroTimer)
          <=> drawCyclesCounter s

drawTimer :: Int -> Widget Name
drawTimer timerDuration =
  C.hCenter $
    padTopBottom 2 $
      withAttr timerAttr $
        padTopBottom 1 $
          padLeftRight 1 $
            str $
              formatTimer timerDuration

drawCyclesCounter :: AppState -> Widget Name
drawCyclesCounter s = C.hCenter (label (formatCycleCounter (s ^. pomodoroCycleCounter)))

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

drawTaskListItem :: Bool -> TK.Task -> Widget Name
drawTaskListItem sel task
  | sel =
      withAttr selectedTaskAttr $
        padLeft (Pad 1) $
          taskListItem task
  | otherwise =
      taskListItem task

taskListItem :: TK.Task -> Widget Name
taskListItem task =
  vBox
    [ withBorderStyle BS.unicodeRounded $
        B.border $
          padLeft (Pad 1) $
            txtWrap (task ^. TK.taskContent)
              <=> if task ^. TK.taskCompleted then padTop (Pad 1) $ txtWrap "☒" else padTop (Pad 1) $ txtWrap "☐"
    ]

formatTimer :: Int -> String
formatTimer timer =
  let minutes = timer `div` 60
      seconds = timer `mod` 60
   in printf "%02d:%02d" minutes seconds

formatCycleCounter :: Int -> String
formatCycleCounter =
  printf "Pomodoro cycles: %01d"
