{-# LANGUAGE OverloadedStrings #-}

module UI (uiMain) where

import Brick
  ( App (..),
    Padding (Pad),
    Widget,
    customMain,
    emptyWidget,
    fill,
    padBottom,
    padLeft,
    padLeftRight,
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
import Brick.Widgets.Dialog (renderDialog)
import Brick.Widgets.Edit
  ( editor,
    renderEditor,
  )
import qualified Brick.Widgets.List as BL
import Config (configFileSettings, extractInitialTimerValue, initialTimerSetting, readInitialTimer, createConfigFileIfNotExists, readConfig)
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
    configList,
    focus,
    initialTimerDialog,
    longBreakTimer,
    pomodoroTimer,
    shortBreakTimer,
    taskEditor,
    taskList
  )
import qualified Resources as R
import Task (createTasksFileIfNotExists, readTasks)
import UI.Attributes
  ( attributes,
    selectedTaskAttr,
    taskCompletedLabelAttr,
    taskCompletedWhiteBgLabelAttr,
    taskPendingLabelAttr,
    taskPendingWhiteBgLabelAttr,
    timerAttr,
  )
import UI.Config (drawConfigList, timerDialog)
import UI.EventHandler (handleEvent)
import UI.Timer (drawTimers, formatTimer)

uiMain :: IO ()
uiMain = do
  eventChan <- newBChan 10
  createConfigFileIfNotExists
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
  tasks <- readTasks
  configFile <- readConfig
  setPomodoroInitialTimer <- readInitialTimer R.Pomodoro
  setShortBreakInitialTimer <- readInitialTimer R.ShortBreak
  setLongBreakInitialTimer <- readInitialTimer R.LongBreak
  return
    AppState
      { _timerRunning = False,
        _pomodoroCounter = 0,
        _pomodoroCyclesCounter = 0,
        _pomodoroTimer = setPomodoroInitialTimer,
        _shortBreakTimer = setShortBreakInitialTimer,
        _longBreakTimer = setLongBreakInitialTimer,
        _taskEditor = editor (TaskEdit Insert) (Just 5) "",
        _focus =
          BF.focusRing
            [ TaskList Pomodoro,
              TaskList ShortBreak,
              TaskList LongBreak,
              TaskEdit Insert,
              TaskEdit Edit,
              Commands,
              Config,
              InitialTimerDialog Pomodoro,
              InitialTimerDialog ShortBreak,
              InitialTimerDialog LongBreak
            ],
        _taskList = BL.list (TaskList Pomodoro) (DV.fromList tasks) 1,
        _configList = BL.list Config (DV.fromList $ configFileSettings configFile) 1,
        _initialTimerDialog = timerDialog (Just 0) Pomodoro
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
    fcs@(Just (TaskEdit _)) -> [B.border (C.center $ drawTimers s <=> drawTaskList (s ^. taskList) <=> drawTaskEditor s) <=> drawCommands fcs]
    Just Commands -> [B.border $ C.center drawCommandsScreen]
    fcs@(Just Config) -> [B.border (C.center $ drawConfigList (s ^. configList)) <=> drawCommands fcs]
    Just (InitialTimerDialog timer) -> do
      let configListL = DV.toList $ BL.listElements (s ^. configList)
          (_, currentInitialTimerValue) =
            maybe
              (Pomodoro, 0)
              extractInitialTimerValue
              (initialTimerSetting timer configListL)
      [ B.border $
          C.center $
            drawConfigList (s ^. configList)
              <=> renderDialog
                (s ^. initialTimerDialog)
                ( vBox
                    [ padTop (Pad 1) $
                        C.hCenter (txt "Initial timer")
                          <=> C.hCenter (withAttr timerAttr (padLeftRight 1 $ str $ formatTimer currentInitialTimerValue)),
                      padTop (Pad 1) $
                        C.hCenter (txt "Active timer value")
                          <=> C.hCenter
                            ( padBottom (Pad 1) $ case timer of
                                Pomodoro -> C.hCenter $ padLeftRight 1 $ str $ formatTimer $ s ^. pomodoroTimer
                                ShortBreak -> C.hCenter $ padLeftRight 1 $ str $ formatTimer $ s ^. shortBreakTimer
                                LongBreak -> C.hCenter $ padLeftRight 1 $ str $ formatTimer $ s ^. longBreakTimer
                            ),
                      C.hCenter $ txt "[Up arrow]   - Increase by 1min",
                      C.hCenter $ padBottom (Pad 1) $ txt "[Down arrow] - Decrease by 1min"
                    ]
                )
              <=> fill ' '
        ]
    _ -> [B.border $ C.center $ drawTimers s <=> drawTaskList (s ^. taskList) <=> drawCommandsTip]

drawCommandsTip :: Widget Name
drawCommandsTip = C.hCenter $ str "press c to see the commands"

drawCommandsScreen :: Widget Name
drawCommandsScreen =
  C.hCenter $
    str
      " [q]           - Quit application \n \
      \[Shift + Tab] - Go to next timer \n \
      \[s]           - Start/Stop timer\n \
      \[r]           - Reset timer\n \
      \[t]           - Add a task\n \
      \[e]           - Edit a task\n \
      \[Del]         - Delete a task\n \
      \[Ctrl + C]    - Change a task's status\n \
      \[p]           - Configuration menu\n"

drawCommands :: Maybe Name -> Widget Name
drawCommands currentFocus = do
  case currentFocus of
    Just (TaskEdit Insert) -> strWrap "ESC: Cancel task creation  Ins: Create task"
    Just (TaskEdit Edit) -> strWrap "ESC: Cancel task edition  Ins: Save task"
    Just Config -> str "ENTER: Select/Toggle setting"
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