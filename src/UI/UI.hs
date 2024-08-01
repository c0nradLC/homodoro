{-# LANGUAGE OverloadedStrings #-}

module UI.UI (uiMain) where

import Brick
  ( App (..),
    Padding (Pad),
    Widget,
    customMain,
    emptyWidget,
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
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Edit
  ( editor,
    renderEditor,
  )
import qualified Brick.Widgets.List as BL
import Config (getInitialTimer)
import qualified Config as CFG (createConfigFileIfNotExists)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad.State
  ( forever,
    void,
  )
import qualified Data.Text as Txt
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import Resources
  ( AppState (..),
    Name (..),
    Task,
    TaskAction (Edit, Insert),
    Tick (..),
    Timer (LongBreak, Pomodoro, ShortBreak),
    focus,
    taskEditor,
    taskList,
  )
import qualified Resources as R
import Task (createTasksFileIfNotExists, getTasks)
import UI.Attributes
  ( attributes,
    selectedTaskAttr,
    taskCompletedLabelAttr,
    taskCompletedWhiteBgLabelAttr,
    taskPendingLabelAttr,
    taskPendingWhiteBgLabelAttr,
  )
import UI.EventHandler (handleEvent)
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
    let taskListWidget = BL.list (TaskList Pomodoro) (DV.fromList tasks) 1
     in AppState
          { _timerRunning = False,
            _pomodoroCounter = 0,
            _pomodoroCyclesCounter = 0,
            _pomodoroTimer = pomodoroTimerDuration,
            _shortBreakTimer = shortBreakTimerDuration,
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

drawUI :: AppState -> [Widget Name]
drawUI s =
  case BF.focusGetCurrent (s ^. focus) of
    fcs@(Just (TaskEdit _)) -> [B.border $ C.center $ drawTimers s <=> drawTaskList (s ^. taskList) <=> drawTaskEditor s <=> drawCommands fcs]
    Just Commands -> [B.border $ C.center drawCommandsScreen]
    _ -> [B.border $ C.center $ drawTimers s <=> drawTaskList (s ^. taskList) <=> drawCommandsTip]

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
      \I/D         -> Increase/Decrease timer by 10sec\n \
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

drawTaskList :: BL.List Name Task -> Widget Name
drawTaskList tasks = do
  B.borderWithLabel (str "Tasks") $
    BL.renderList drawTaskListItem True tasks

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
