{-# LANGUAGE OverloadedStrings #-}

module UI (uiMain) where

import Brick (
    App (..),
    Widget,
    customMain,
    emptyWidget,
    fill,
    showFirstCursor,
    str,
    strWrap,
    (<=>),
 )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Dialog (renderDialog)
import Brick.Widgets.Edit (
    editor,
 )
import qualified Brick.Widgets.List as BL
import Config (configFileSettings, createConfigFileIfNotExists, extractInitialTimerValue, findInitialTimerSetting, readConfig, readInitialTimer)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad.State (
    forever,
    void,
 )
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import Resources (
    AppState (..),
    Name (..),
    TaskAction (Edit, Insert),
    Tick (..),
    Timer (LongBreak, Pomodoro, ShortBreak),
    configList,
    focus,
    initialTimerDialog,
    taskList,
 )
import qualified Resources as R
import Task (createTasksFileIfNotExists, readTasks)
import UI.Attributes (
    attributes,
 )
import UI.Config (drawConfigList)
import UI.EventHandler (handleEvent)
import UI.Task
import UI.Timer (drawInitialTimerDialog, drawTimers, timerDialog)

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
            { _timerRunning = False
            , _pomodoroCounter = 0
            , _pomodoroCyclesCounter = 0
            , _pomodoroTimer = setPomodoroInitialTimer
            , _shortBreakTimer = setShortBreakInitialTimer
            , _longBreakTimer = setLongBreakInitialTimer
            , _taskEditor = editor (TaskEdit Insert) (Just 5) ""
            , _focus =
                BF.focusRing
                    [ TaskList Pomodoro
                    , TaskList ShortBreak
                    , TaskList LongBreak
                    , TaskEdit Insert
                    , TaskEdit Edit
                    , Config
                    , InitialTimerDialog Pomodoro
                    , InitialTimerDialog ShortBreak
                    , InitialTimerDialog LongBreak
                    ]
            , _taskList = BL.list (TaskList Pomodoro) (DV.fromList tasks) 1
            , _configList = BL.list Config (DV.fromList $ configFileSettings configFile) 1
            , _initialTimerDialog = timerDialog (Just 0) Pomodoro
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

drawUI :: AppState -> [Widget Name]
drawUI s =
    case BF.focusGetCurrent (s ^. focus) of
        fcs@(Just (TaskEdit _)) -> [B.border (C.center $ drawTimers s <=> drawTaskList (s ^. taskList) <=> drawTaskEditor s) <=> drawCommands fcs]
        fcs@(Just Config) -> [B.border (C.center $ drawConfigList (s ^. configList)) <=> drawCommands fcs]
        Just (InitialTimerDialog timer) -> do
            let configListL = DV.toList $ BL.listElements (s ^. configList)
                currentInitialTimerValue =
                    extractInitialTimerValue
                    $ findInitialTimerSetting timer configListL
            [ B.border $
                    C.center $
                        drawConfigList (s ^. configList)
                            <=> renderDialog
                                (s ^. initialTimerDialog)
                                (drawInitialTimerDialog currentInitialTimerValue)
                            <=> fill ' '
                ]
        fcs -> [B.border (C.center $ drawTimers s <=> drawTaskList (s ^. taskList)) <=> drawCommands fcs]

drawCommands :: Maybe Name -> Widget Name
drawCommands currentFocus = do
    case currentFocus of
        Just (TaskEdit Insert) -> strWrap "[ESC]: cancel task creation, [Ins]: create task"
        Just (TaskEdit Edit) -> strWrap "[ESC]: cancel task edit, [Ins]: save task"
        Just (TaskList _) ->
            strWrap
                "[Q]: quit, [S]: Start/Stop, [R]: reset, [Shift + Tab]: next timer, \
                \[T]: add task, [E]: edit task, [Ctrl + C]: toggle task status, \
                \[P]: config menu"
        Just Config -> str "[ESC]: return, [ENTER]: select/Toggle setting"
        _ -> emptyWidget