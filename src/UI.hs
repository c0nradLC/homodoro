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
import Brick.Widgets.FileBrowser (
    newFileBrowser,
    renderFileBrowser,
    selectNonDirectories,
 )
import qualified Brick.Widgets.List as BL
import Config (configBoolValue, configFilePathValue, configFileSettings, configIntValue, createProgramFileAndDirectoriesIfNotExists, readConfig, showBool, soundVolumePercentage)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad.State (
    forever,
    void
 )
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import Task (readTasks)
import Types (
    AppState (..),
    Name (..),
    TaskAction (Edit, Insert),
    Tick (..),
    Timer (LongBreak, Pomodoro, ShortBreak),
    TimerState (TimerState, _timerCurrentValue, _timerInitialValue),
    Timers (Timers, _longBreakState, _pomodoroState, _shortBreakState, _timerCurrentFocus),
    audioDirectoryPathBrowser,
    audioDirectoryPathSetting,
    configList,
    focus,
    initialTimerConfigDialog,
    longBreakInitialTimerSetting,
    longBreakState,
    pomodoroInitialTimerSetting,
    pomodoroState,
    shortBreakInitialTimerSetting,
    shortBreakState,
    taskList,
    tasksFilePathBrowser,
    tasksFilePathSetting,
    timerAlertSoundVolume,
    timerAlertSoundVolumeConfigDialog,
    timerAlertSoundVolumeSetting,
    timerInitialValue,
    timerPopupAlert,
    timerPopupAlertSetting,
    timerStartStopSoundVolume,
    timerStartStopSoundVolumeConfigDialog,
    timerStartStopSoundVolumeSetting,
    timerTickSoundVolume,
    timerTickSoundVolumeConfigDialog,
    timerTickSoundVolumeSetting,
    timers, AudioCache (AudioCache), Audio (TimerTick, TimerAlert, TimerStartStop), audioFilesFound,
 )
import UI.Attributes (
    attributes,
 )
import UI.Config (drawConfigList, drawInitialTimerDialog, drawSoundVolumeDialog, initialTimerDialog, soundVolumeDialog)
import UI.EventHandler (handleEvent)
import UI.Task (drawTaskEditor, drawTaskList)
import UI.Timer (drawTimers)
import SDL (initializeAudio, preloadAllAudio)
import qualified Data.Map as Map
import Data.IORef (newIORef)

uiMain :: IO ()
uiMain = do
    eventChan <- newBChan 10
    createProgramFileAndDirectoriesIfNotExists
    _ <- initializeAudio
    _ <- forkIO $ forever $ do
        writeBChan eventChan Tick
        threadDelay 1000000
    initialState <- createAppState
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just eventChan) app initialState

createAppState :: IO AppState
createAppState = do
    configFile <- readConfig
    tasks <- readTasks $ configFilePathValue $ configFile ^. tasksFilePathSetting
    cacheRef <- newIORef Map.empty
    let setTimerStartStopSoundVolume = configIntValue $ configFile ^. timerStartStopSoundVolumeSetting
        setAudioDirectoryPath = configFilePathValue $ configFile ^. audioDirectoryPathSetting
        setPomodoroInitialTimer = configIntValue $ configFile ^. pomodoroInitialTimerSetting
        setShortBreakInitialTimer = configIntValue $ configFile ^. shortBreakInitialTimerSetting
        setLongBreakInitialTimer = configIntValue $ configFile ^. longBreakInitialTimerSetting
        setTasksFilePath = configFilePathValue $ configFile ^. tasksFilePathSetting
        setAlertSoundVolume = configIntValue $ configFile ^. timerAlertSoundVolumeSetting
        setTimerTickSoundVolume = configIntValue $ configFile ^. timerTickSoundVolumeSetting
        setTimerPopupAlert = configBoolValue $ configFile ^. timerPopupAlertSetting
        initialAudioManager = AudioCache cacheRef
        setTimerValues =
            Timers
                { _pomodoroState = TimerState{_timerCurrentValue = setPomodoroInitialTimer, _timerInitialValue = setPomodoroInitialTimer}
                , _shortBreakState = TimerState{_timerCurrentValue = setShortBreakInitialTimer, _timerInitialValue = setShortBreakInitialTimer}
                , _longBreakState = TimerState{_timerCurrentValue = setLongBreakInitialTimer, _timerInitialValue = setLongBreakInitialTimer}
                , _timerCurrentFocus = Pomodoro
                }
     in do
            initialTasksFilePathBrowser <- newFileBrowser selectNonDirectories TasksFilePathBrowser $ Just setTasksFilePath
            initialAudioDirectoryPathBrowser <- newFileBrowser selectNonDirectories AudioDirectoryPathBrowser $ Just setAudioDirectoryPath
            loadedAudio <- preloadAllAudio initialAudioManager setAudioDirectoryPath
            return
                AppState
                    { _timerRunning = False
                    , _timers = setTimerValues
                    , _pomodoroCounter = 0
                    , _pomodoroCyclesCounter = 0
                    , _taskEditor = editor (TaskEdit Insert) (Just 5) ""
                    , _focus =
                        BF.focusRing
                            [ TaskList
                            , TaskEdit Insert
                            , TaskEdit Edit
                            , Config
                            , InitialTimerDialog Pomodoro
                            , InitialTimerDialog ShortBreak
                            , InitialTimerDialog LongBreak
                            , TasksFilePathBrowser
                            , TimerAlertSoundVolumeDialog
                            , TimerTickSoundVolumeDialog
                            , TimerStartStopSoundVolumeDialog
                            , AudioDirectoryPathBrowser
                            ]
                    , _taskList = BL.list TaskList (DV.fromList tasks) 1
                    , _configList = BL.list Config (DV.fromList $ configFileSettings configFile) 1
                    , _configFile = configFile
                    , _initialTimerConfigDialog = initialTimerDialog Pomodoro
                    , _tasksFilePath = setTasksFilePath
                    , _tasksFilePathBrowser = initialTasksFilePathBrowser
                    , _timerPopupAlert = setTimerPopupAlert
                    , _timerStartStopSoundVolume = setTimerStartStopSoundVolume
                    , _timerStartStopSoundVolumeConfigDialog = soundVolumeDialog (Just "Timer start/stop sound volume")
                    , _timerAlertSoundVolume = setAlertSoundVolume
                    , _timerAlertSoundVolumeConfigDialog = soundVolumeDialog (Just "Timer alert sound volume")
                    , _timerTickSoundVolume = setTimerTickSoundVolume
                    , _timerTickSoundVolumeConfigDialog = soundVolumeDialog (Just "Timer tick sound volume")
                    , _audioDirectoryPath = setAudioDirectoryPath
                    , _audioDirectoryPathBrowser = initialAudioDirectoryPathBrowser
                    , _audioCache = initialAudioManager
                    , _audioFilesFound = map fst loadedAudio
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
drawUI s = do
    case BF.focusGetCurrent (s ^. focus) of
        currentFocus@(Just (TaskEdit _)) -> [B.border (C.center $ drawHeader s <=> drawTimers s <=> drawTaskList (s ^. taskList) <=> drawTaskEditor s) <=> drawCommands currentFocus]
        currentFocus@(Just Config) -> [B.border (C.center $ drawConfigList (s ^. configList)) <=> drawCommands currentFocus]
        currentFocus@(Just (InitialTimerDialog timer)) ->
            let currentInitialTimerValue = case timer of
                    Pomodoro -> s ^. timers . pomodoroState . timerInitialValue
                    ShortBreak -> s ^. timers . shortBreakState . timerInitialValue
                    LongBreak -> s ^. timers . longBreakState . timerInitialValue
             in [ B.border $
                    C.center $
                        drawConfigList (s ^. configList)
                            <=> renderDialog
                                (s ^. initialTimerConfigDialog)
                                (drawInitialTimerDialog currentInitialTimerValue)
                            <=> fill ' '
                            <=> drawCommands currentFocus
                ]
        Just TimerTickSoundVolumeDialog ->
            [ B.border $
                C.center $
                    drawConfigList (s ^. configList)
                        <=> renderDialog
                            (s ^. timerTickSoundVolumeConfigDialog)
                            (drawSoundVolumeDialog "Current timer tick sound volume" (s ^. timerTickSoundVolume))
                        <=> fill ' '
            ]
        Just TimerAlertSoundVolumeDialog ->
            [ B.border $
                C.center $
                    drawConfigList (s ^. configList)
                        <=> renderDialog
                            (s ^. timerAlertSoundVolumeConfigDialog)
                            (drawSoundVolumeDialog "Current timer alert sound volume" (s ^. timerAlertSoundVolume))
                        <=> fill ' '
            ]
        Just TimerStartStopSoundVolumeDialog ->
            [ B.border $
                C.center $
                    drawConfigList (s ^. configList)
                        <=> renderDialog
                            (s ^. timerStartStopSoundVolumeConfigDialog)
                            (drawSoundVolumeDialog "Current timer start/stop sound volume" (s ^. timerStartStopSoundVolume))
                        <=> fill ' '
            ]
        currentFocus@(Just TasksFilePathBrowser) ->
            [ B.border
                ( C.center $
                    drawConfigList (s ^. configList)
                        <=> B.border (renderFileBrowser True (s ^. tasksFilePathBrowser))
                        <=> drawCommands currentFocus
                )
            ]
        currentFocus@(Just AudioDirectoryPathBrowser) ->
            [ B.border
                ( C.center $
                    drawConfigList (s ^. configList)
                        <=> B.border (renderFileBrowser True (s ^. audioDirectoryPathBrowser))
                        <=> drawCommands currentFocus
                )
            ]
        currentFocus ->
            [B.border (C.center $ drawHeader s <=> drawTimers s <=> drawTaskList (s ^. taskList)) <=> drawCommands currentFocus]
  where
    drawCommands currentFocus =
        case currentFocus of
            Just (TaskEdit Insert) -> strWrap "[ESC]: cancel task creation, [INS]: create task"
            Just (TaskEdit Edit) -> strWrap "[ESC]: cancel task edit, [INS]: save task"
            Just TaskList ->
                strWrap
                    "[Q]: quit, [S]: start/stop timer, [R]: reset timer, [SHIFT + TAB]: next timer, \
                    \[T]: add task, [E]: edit task, [Ctrl + C]: toggle task status, \
                    \[P]: config menu"
            Just Config -> str "[ESC|Q]: return, [ENTER]: select/toggle setting"
            Just TasksFilePathBrowser ->
                strWrap
                    "[ESC|Q]: close, [ENTER]: choose selection, [/]: search"
            Just AudioDirectoryPathBrowser ->
                strWrap
                    "[ESC|Q]: close, [C]: choose selection, [SHIFT + C]: choose current directory, [/]: search"
            _ -> emptyWidget
    drawHeader state = do
        str ("Timer popup: " ++ showBool (state ^. timerPopupAlert))
            <=> str ("Timer tick volume: " ++ (if TimerTick `elem` (s ^. audioFilesFound) then
                                                    soundVolumePercentage (state ^. timerTickSoundVolume)
                                                else "Audio not found"))
            <=> str ("Timer alert volume: " ++ (if TimerAlert `elem` (s ^. audioFilesFound) then
                                                    soundVolumePercentage (state ^. timerAlertSoundVolume)
                                                else "Audio not found"))
            <=> str ("Timer start/stop volume: " ++ if TimerStartStop `elem` (s ^. audioFilesFound) then
                                                    soundVolumePercentage (state ^. timerStartStopSoundVolume)
                                                    else "Audio not found")
