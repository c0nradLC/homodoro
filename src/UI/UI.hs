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
import Config (getInitialTimer, configFileSettings)
import qualified Config as CFG (createConfigFileIfNotExists, getConfig)
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
    taskList, configList,
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
import UI.Config (drawConfigList, timerDialog)
import Brick.Widgets.Dialog (renderDialog)

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
  configFile   <- CFG.getConfig
  setPomodoroInitialTimer <- getInitialTimer R.Pomodoro
  setShortBreakInitialTimer <- getInitialTimer R.ShortBreak
  setLongBreakInitialTimer <- getInitialTimer R.LongBreak
  return AppState
    { _timerRunning = False,
      _pomodoroCounter = 0,
      _pomodoroCyclesCounter = 0,
      _pomodoroTimer = setPomodoroInitialTimer,
      _shortBreakTimer = setShortBreakInitialTimer,
      _longBreakTimer = setLongBreakInitialTimer,
      _taskEditor = editor (TaskEdit Insert) (Just 5) "",
      _focus = BF.focusRing [ TaskList Pomodoro, TaskList ShortBreak, TaskList LongBreak,
                              TaskEdit Insert, TaskEdit Edit,
                              Commands, Config,
                              InitialTimerDialog Pomodoro, InitialTimerDialog ShortBreak, InitialTimerDialog LongBreak
                            ],
      _taskList = BL.list (TaskList Pomodoro) (DV.fromList tasks) 1,
      _configList = BL.list Config (DV.fromList $ configFileSettings configFile) 1,
      _initialTimerDialog = Nothing
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
    Just Config -> [B.border $ C.center $ drawConfigList (s ^. configList)]
    Just (InitialTimerDialog timer) -> [B.border $ C.center $ renderDialog (timerDialog timer) emptyWidget]
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
      \Ctrl + C    -> Change a task's status\n \
      \p           -> Configuration menu\n"

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
