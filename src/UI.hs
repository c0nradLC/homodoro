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
import Config (configFileSettings, createProgramFileAndDirectoriesIfNotExists, findConfigSetting, maybeConfigBoolValue, maybeConfigIntValue, readAlertSoundVolume, readAudioDirectoryPath, readConfig, readInitialTimer, readTasksFilePath, readTimerTickSoundVolume, showBool)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad.State (
    forever,
    void,
 )
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import Process (getAudioProvider, getNotificationProvider)
import Task (readTasks)
import Types (
    AppState (..),
    ConfigSettingValue (..),
    Name (..),
    TaskAction (Edit, Insert),
    Tick (..),
    Timer (LongBreak, Pomodoro, ShortBreak),
    audioDirectoryPathBrowser,
    configList,
    focus,
    initialTimerConfigDialog,
    taskList,
    tasksFilePathBrowser,
    timerAlertSoundVolume,
    timerAlertSoundVolumeConfigDialog,
    timerTickSoundVolume,
    timerTickSoundVolumeConfigDialog,
 )
import UI.Attributes (
    attributes,
 )
import UI.Config (drawConfigList, drawInitialTimerDialog, drawSoundVolumeDialog, initialTimerDialog, soundVolumeDialog)
import UI.EventHandler (handleEvent)
import UI.Task (drawTaskEditor, drawTaskList)
import UI.Timer (drawTimers)

uiMain :: IO ()
uiMain = do
    eventChan <- newBChan 10
    createProgramFileAndDirectoriesIfNotExists
    _ <- forkIO $ forever $ do
        writeBChan eventChan Tick
        threadDelay 1000000
    initialState <- createAppState
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just eventChan) app initialState

createAppState :: IO AppState
createAppState = do
    tasks <- readTasks
    configFile <- readConfig
    setPomodoroInitialTimer <- readInitialTimer Pomodoro
    setShortBreakInitialTimer <- readInitialTimer ShortBreak
    setLongBreakInitialTimer <- readInitialTimer LongBreak
    tasksFilePathSetting <- readTasksFilePath
    initialTasksFilePathBrowser <- newFileBrowser selectNonDirectories TasksFilePathBrowser $ Just tasksFilePathSetting
    setAudioDirectoryPath <- readAudioDirectoryPath
    initialAudioDirectoryPathBrowser <- newFileBrowser selectNonDirectories AudioDirectoryPathBrowser $ Just setAudioDirectoryPath
    setAlertSoundVolume <- readAlertSoundVolume
    setTimerTickSoundVolume <- readTimerTickSoundVolume
    availableNotificationProvider <- getNotificationProvider
    availableAudioProvider <- getAudioProvider
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
                    , TimerAlertSoundVolumeDialog
                    , TimerTickSoundVolumeDialog
                    , AudioDirectoryPathBrowser
                    ]
            , _taskList = BL.list (TaskList Pomodoro) (DV.fromList tasks) 1
            , _configList = BL.list Config (DV.fromList $ configFileSettings configFile) 1
            , _initialTimerConfigDialog = initialTimerDialog (Just 0) Pomodoro
            , _tasksFilePath = tasksFilePathSetting
            , _tasksFilePathBrowser = initialTasksFilePathBrowser
            , _timerAlertSoundVolume = setAlertSoundVolume
            , _timerAlertSoundVolumeConfigDialog = soundVolumeDialog (Just "Timer alert sound volume") $ Just setAlertSoundVolume
            , _timerTickSoundVolume = setTimerTickSoundVolume
            , _timerTickSoundVolumeConfigDialog = soundVolumeDialog (Just "Timer tick sound volume") $ Just setTimerTickSoundVolume
            , _notificationProvider = availableNotificationProvider
            , _audioProvider = availableAudioProvider
            , _audioDirectoryPath = setAudioDirectoryPath
            , _audioDirectoryPathBrowser = initialAudioDirectoryPathBrowser
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
        currentFocus@(Just (TaskEdit _)) -> [B.border (C.center $ drawHeader (s ^. timerAlertSoundVolume) (s ^. timerTickSoundVolume) <=> drawTimers s <=> drawTaskList (s ^. taskList) <=> drawTaskEditor s) <=> drawCommands currentFocus]
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
        currentFocus@(Just TimerTickSoundVolumeDialog) -> do
            let currentTimerTickSoundVolumeConfig =
                    maybeConfigIntValue $ findConfigSetting (ConfigTimerTickSoundVolume 0) configListL
            [ B.border $
                    C.center $
                        drawConfigList (s ^. configList)
                            <=> renderDialog
                                (s ^. timerTickSoundVolumeConfigDialog)
                                (drawSoundVolumeDialog "Current timer tick sound volume" currentTimerTickSoundVolumeConfig)
                            <=> fill ' '
                            <=> drawCommands currentFocus
                ]
        currentFocus@(Just TimerAlertSoundVolumeDialog) -> do
            let currentAlertSoundVolumeConfig =
                    maybeConfigIntValue $ findConfigSetting (ConfigTimerAlertSoundVolume 0) configListL
            [ B.border $
                    C.center $
                        drawConfigList (s ^. configList)
                            <=> renderDialog
                                (s ^. timerAlertSoundVolumeConfigDialog)
                                (drawSoundVolumeDialog "Current timer alert sound volume" currentAlertSoundVolumeConfig)
                            <=> fill ' '
                            <=> drawCommands currentFocus
                ]
        currentFocus@(Just TasksFilePathBrowser) -> do
            [ B.border
                    ( C.center $
                        drawConfigList (s ^. configList)
                            <=> B.border (renderFileBrowser True (s ^. tasksFilePathBrowser))
                            <=> drawCommands currentFocus
                    )
                ]
        currentFocus@(Just AudioDirectoryPathBrowser) -> do
            [ B.border
                    ( C.center $
                        drawConfigList (s ^. configList)
                            <=> B.border (renderFileBrowser True (s ^. audioDirectoryPathBrowser))
                            <=> drawCommands currentFocus
                    )
                ]
        currentFocus -> do
            [B.border (C.center $ drawHeader (s ^. timerAlertSoundVolume) (s ^. timerTickSoundVolume) <=> drawTimers s <=> drawTaskList (s ^. taskList)) <=> drawCommands currentFocus]
  where
    configListL = DV.toList $ BL.listElements (s ^. configList)
    drawCommands currentFocus =
        case currentFocus of
            Just (TaskEdit Insert) -> strWrap "[ESC]: cancel task creation, [INS]: create task"
            Just (TaskEdit Edit) -> strWrap "[ESC]: cancel task edit, [INS]: save task"
            Just (TaskList _) ->
                strWrap
                    "[Q]: quit, [S]: Start/Stop, [R]: reset, [SHIFT + TAB]: next timer, \
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
    drawHeader alertVolume timerTickVolume = do
        let popupEnabled = maybeConfigBoolValue $ findConfigSetting (ConfigTimerPopupAlert False) configListL
         in str ("Timer popup: " ++ showBool popupEnabled)
                <=> (if timerTickVolume > 0 then str ("Timer tick volume: " ++ show timerTickVolume) else emptyWidget)
                <=> if alertVolume > 0
                    then str ("Timer alert volume: " ++ show alertVolume)
                    else emptyWidget
