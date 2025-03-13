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
import Config (configFileSettings, createConfigFileIfNotExists, readConfig, readInitialTimer, readTasksFilePath, maybeConfigIntValue, readSoundVolume, findConfigSetting, showBool, maybeConfigBoolValue, soundVolumePercentage)
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
    initialTimerConfigDialog,
    taskList, tasksFilePathBrowser, currentSoundVolume, soundVolumeConfigDialog, ConfigSettingValue (..),
 )
import qualified Resources as R
import Task (createTasksFileIfNotExists, readTasks)
import UI.Attributes (
    attributes
 )
import UI.Config (drawConfigList, initialTimerDialog, drawInitialTimerDialog, soundVolumeDialog, drawSoundVolumeDialog)
import UI.EventHandler (handleEvent)
import UI.Task
import UI.Timer (drawTimers)
import Brick.Widgets.FileBrowser

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
    tasksFilePath <- readTasksFilePath
    initialTasksFilePathBrowser <- newFileBrowser selectNonDirectories TasksFilePathBrowser $ Just tasksFilePath
    setSoundVolume <- readSoundVolume
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
                    , TasksFilePathBrowser
                    , SoundVolumeDialog
                    ]
            , _taskList = BL.list (TaskList Pomodoro) (DV.fromList tasks) 1
            , _configList = BL.list Config (DV.fromList $ configFileSettings configFile) 1
            , _initialTimerConfigDialog = initialTimerDialog (Just 0) Pomodoro
            , _tasksFilePathBrowser = initialTasksFilePathBrowser
            , _currentSoundVolume = setSoundVolume
            , _soundVolumeConfigDialog = soundVolumeDialog $ Just setSoundVolume
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
        currentFocus@(Just (TaskEdit _)) -> [B.border (C.center $ drawHeader (s ^. currentSoundVolume)  <=> drawTimers s <=> drawTaskList (s ^. taskList) <=> drawTaskEditor s) <=> drawCommands currentFocus]
        currentFocus@(Just Config) -> [B.border (C.center $ drawConfigList (s ^. configList)) <=> drawCommands currentFocus]
        currentFocus@(Just (InitialTimerDialog timer)) -> do
            let currentInitialTimerValue =
                    maybeConfigIntValue $ findConfigSetting (ConfigInitialTimer timer 0) configListL
            [ B.border $
                    C.center $
                            drawConfigList (s ^. configList)
                            <=> renderDialog
                                (s ^. initialTimerConfigDialog)
                                (drawInitialTimerDialog currentInitialTimerValue)
                            <=> fill ' '
                            <=> drawCommands currentFocus
                ]
        currentFocus@(Just SoundVolumeDialog) -> do
            let currentSoundVolumeConfig =
                    maybeConfigIntValue $ findConfigSetting (ConfigSoundVolume 0) configListL
            [ B.border $
                    C.center $
                            drawConfigList (s ^. configList)
                            <=> renderDialog
                                (s ^. soundVolumeConfigDialog)
                                (drawSoundVolumeDialog currentSoundVolumeConfig)
                            <=> fill ' '
                            <=> drawCommands currentFocus
                ]
        currentFocus@(Just TasksFilePathBrowser) -> do
            [ B.border
                    (C.center $
                            drawConfigList (s ^. configList)
                            <=> B.border (renderFileBrowser True (s ^. tasksFilePathBrowser))
                            <=> drawCommands currentFocus)
                ]
        currentFocus -> do 
            [B.border (C.center $ drawHeader (s ^. currentSoundVolume)  <=> drawTimers s <=> drawTaskList (s ^. taskList)) <=> drawCommands currentFocus]
    where
        configListL = DV.toList $ BL.listElements (s ^. configList)
        drawCommands currentFocus =
            case currentFocus of
                Just (TaskEdit Insert) -> strWrap "[ESC]: cancel task creation, [Ins]: create task"
                Just (TaskEdit Edit) -> strWrap "[ESC]: cancel task edit, [Ins]: save task"
                Just (TaskList _) ->
                    strWrap
                        "[Q]: quit, [S]: Start/Stop, [R]: reset, [Shift + Tab]: next timer, \
                        \[T]: add task, [E]: edit task, [Ctrl + C]: toggle task status, \
                        \[P]: config menu"
                Just Config -> str "[ESC|Q]: return, [ENTER]: select/toggle setting"
                Just TasksFilePathBrowser ->
                    strWrap
                    "[ESC|Q]: close, [ENTER]: select entry, [/]: search"
                _ -> emptyWidget
        drawHeader vol = do
            let popupEnabled = maybeConfigBoolValue $ findConfigSetting (ConfigTimerNotificationAlert False) configListL
                soundAlertEnabled = maybeConfigBoolValue $ findConfigSetting (ConfigTimerSoundAlert False) configListL
                in
                str ("Vol: " ++ soundVolumePercentage vol)
                <=> str ("Timer sound: " ++ showBool soundAlertEnabled)
                <=> str ("Timer notification: " ++ showBool popupEnabled)