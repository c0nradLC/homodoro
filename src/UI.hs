{-# LANGUAGE OverloadedStrings #-}

module UI (uiMain) where

import Brick (
    App (..),
    Padding (Max),
    Widget,
    customMain,
    emptyWidget,
    padBottom,
    padLeft,
    showFirstCursor,
    str,
    strWrap,
    (<+>),
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
import Config (configBoolValue, configFilePathValue, configFileSettings, configIntValue, defaultConfig, readConfigFile, showBool, soundVolumePercentage, xdgConfigFilePath)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad.State (
    forever,
    unless,
    void,
 )
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import Persistence (defaultPersistence, readPersistence, writePersistence, xdgPersistenceFilePath)
import SDL (initializeAudio, preloadAllAudio)
import qualified System.Directory as D
import qualified System.FilePath as FP
import Task (readTasks)
import Types (
    AppState (..),
    AudioCache (AudioCache),
    Name (..),
    TaskAction (Edit, Insert),
    Tick (..),
    Timer (LongBreak, Pomodoro, ShortBreak),
    audioDirectoryPathBrowser,
    audioDirectoryPathSetting,
    configList,
    focus,
    focusedTimePersisted,
    initialTimerConfigDialog,
    longBreakInitialTimerSetting,
    longBreakState,
    persistenceFile,
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
    timersPersisted,
    timersPersisted,
 )
import UI.Attributes (
    attributes,
 )
import UI.Config (drawConfigList, drawInitialTimerDialog, drawSoundVolumeDialog, initialTimerDialog, soundVolumeDialog)
import UI.EventHandler (handleEvent)
import UI.Task (drawTaskEditor, drawTaskList)
import UI.Timer (drawTimers, formatTimer)

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
    configFile <- readConfigFile
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
     in do
            currentPersistenceFile <- readPersistence (setPomodoroInitialTimer, setShortBreakInitialTimer, setLongBreakInitialTimer)
            initialTasksFilePathBrowser <- newFileBrowser selectNonDirectories TasksFilePathBrowser $ Just setTasksFilePath
            initialAudioDirectoryPathBrowser <- newFileBrowser selectNonDirectories AudioDirectoryPathBrowser $ Just setAudioDirectoryPath
            void $ preloadAllAudio initialAudioManager setAudioDirectoryPath
            return
                AppState
                    { _timerRunning = False
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
                    , _persistenceFile = currentPersistenceFile
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
        Just (InitialTimerDialog timer) ->
            let currentInitialTimerValue = case timer of
                    Pomodoro -> s ^. persistenceFile . timersPersisted . pomodoroState . timerInitialValue
                    ShortBreak -> s ^. persistenceFile . timersPersisted . shortBreakState . timerInitialValue
                    LongBreak -> s ^. persistenceFile . timersPersisted . longBreakState . timerInitialValue
             in [ B.border $
                    drawConfigList (s ^. configList)
                        <=> padBottom
                            Max
                            ( renderDialog
                                (s ^. initialTimerConfigDialog)
                                (drawInitialTimerDialog currentInitialTimerValue)
                            )
                ]
        Just TimerTickSoundVolumeDialog ->
            [ B.border $
                drawConfigList (s ^. configList)
                    <=> padBottom
                        Max
                        ( renderDialog
                            (s ^. timerTickSoundVolumeConfigDialog)
                            (drawSoundVolumeDialog "Current timer tick sound volume" (s ^. timerTickSoundVolume))
                        )
            ]
        Just TimerAlertSoundVolumeDialog ->
            [ B.border $
                drawConfigList (s ^. configList)
                    <=> padBottom
                        Max
                        ( renderDialog
                            (s ^. timerAlertSoundVolumeConfigDialog)
                            (drawSoundVolumeDialog "Current timer alert sound volume" (s ^. timerAlertSoundVolume))
                        )
            ]
        Just TimerStartStopSoundVolumeDialog ->
            [ B.border $
                drawConfigList (s ^. configList)
                    <=> padBottom
                        Max
                        ( renderDialog
                            (s ^. timerStartStopSoundVolumeConfigDialog)
                            (drawSoundVolumeDialog "Current timer start/stop sound volume" (s ^. timerStartStopSoundVolume))
                        )
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
    drawHeader state =
        do
            str ("Timer popup: " ++ showBool (state ^. timerPopupAlert))
            <=> str
                ( "Timer tick volume: "
                    ++ soundVolumePercentage (state ^. timerTickSoundVolume)
                )
            <=> str
                ( "Timer alert volume: "
                    ++  soundVolumePercentage (state ^. timerAlertSoundVolume)
                       
                )
            <=> str
                ( "Timer start/stop volume: "
                    ++ soundVolumePercentage (state ^. timerStartStopSoundVolume)
                )
            <+> padLeft Max (str ("Focused time today: " ++ formatTimer (state ^. persistenceFile . focusedTimePersisted)))

createProgramFileAndDirectoriesIfNotExists :: IO ()
createProgramFileAndDirectoriesIfNotExists = do
    configFilePath <- xdgConfigFilePath
    configFileExists <- D.doesFileExist configFilePath
    defaultConfigFile <- defaultConfig
    persistenceFilePath <- xdgPersistenceFilePath
    persistenceFileExists <- D.doesFileExist persistenceFilePath
    D.createDirectoryIfMissing True (FP.takeDirectory configFilePath)
    configFile <- if configFileExists then readConfigFile else defaultConfig
    unless configFileExists $ do
        let tasksFilePath = configFilePathValue $ defaultConfigFile ^. tasksFilePathSetting
        writeFile configFilePath $ unpack $ encode defaultConfigFile
        D.createDirectoryIfMissing True $ configFilePathValue $ defaultConfigFile ^. audioDirectoryPathSetting
        D.createDirectoryIfMissing True (FP.takeDirectory $ configFilePathValue $ defaultConfigFile ^. audioDirectoryPathSetting)
        taskFileExists <- D.doesFileExist tasksFilePath
        unless taskFileExists $ do writeFile tasksFilePath ""
    defaultPersistenceFile <-
        defaultPersistence
            ( configIntValue $ configFile ^. pomodoroInitialTimerSetting
            , configIntValue $ configFile  ^. shortBreakInitialTimerSetting
            , configIntValue $ configFile ^. longBreakInitialTimerSetting
            )
    D.createDirectoryIfMissing True (FP.takeDirectory persistenceFilePath)
    unless persistenceFileExists $ writePersistence defaultPersistenceFile
