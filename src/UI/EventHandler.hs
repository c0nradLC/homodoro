{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI.EventHandler (handleEvent) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt, zoom)
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Dialog (dialogSelection, handleDialogEvent)
import Brick.Widgets.Edit (editor, getEditContents, handleEditorEvent)
import Brick.Widgets.FileBrowser (FileInfo (fileInfoFilePath, fileInfoFileStatus), FileStatus (fileStatusFileType), FileType (Directory, RegularFile), fileBrowserCursor, fileBrowserIsSearching, getWorkingDirectory, handleFileBrowserEvent)
import Brick.Widgets.List (
    GenericList (listElements, listSelected),
    List,
    handleListEvent,
    handleListEventVi,
    listReplace,
    listSelectedElement,
 )
import Config (configBoolValue, configFilePathValue, configFileSettings, configIntValue, initialTimerSettingL, updateConfig)
import Control.Concurrent (forkIO)
import Control.Lens (Lens', (.=), (^.))
import Control.Monad.State (MonadIO (liftIO), MonadState (..), when)
import Data.Char (toLower)
import Data.List (find)
import Data.Text (Text, null, unlines)
import Data.Vector (fromList, toList)
import Graphics.Vty (Event (EvKey), Key (..), Modifier (..))
import Process (callAudioProvider, callNotificationProvider)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Task (mkTask, readTasks, taskExists, updateTaskList, writeTasks)
import Types (AppState (), Audio (..), AudioProvider (FFPlay, MPV), ConfigFile, ConfigSetting (ConfigSetting, _configLabel, _configValue), ConfigSettingValue (..), InitialTimerDialogChoice (..), Name (..), NotificationProvider (NotifySend, Zenity), SoundVolumeDialogChoice (CloseSoundVolumeDialog, PlayTestAudio, SaveSoundVolume), Task, TaskAction (Edit, Insert), TaskListOperation (AppendTask, ChangeTaskCompletion, DeleteTask, EditTask), Tick (Tick), Timer (LongBreak, Pomodoro, ShortBreak), TimerState (..), audioDirectoryPath, audioDirectoryPathBrowser, audioDirectoryPathSetting, audioProvider, configFile, configList, configValue, focus, initialTimerConfigDialog, longBreakState, notificationProvider, pomodoroCounter, pomodoroCyclesCounter, pomodoroState, shortBreakState, taskContent, taskEditor, taskList, tasksFilePathBrowser, tasksFilePathSetting, timerAlertSoundVolume, timerAlertSoundVolumeConfigDialog, timerAlertSoundVolumeSetting, timerCurrentValue, timerInitialValue, timerPopupAlert, timerPopupAlertSetting, timerRunning, timerStartStopSoundVolume, timerStartStopSoundVolumeConfigDialog, timerStartStopSoundVolumeSetting, timerTickSoundVolume, timerTickSoundVolumeConfigDialog, timerTickSoundVolumeSetting, timers)
import UI.Config (initialTimerDialog, soundVolumeDialog)
import UI.Util (changeFocus)
import Prelude hiding (null, unlines)

handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent ev = do
    s <- get
    let currentFocus = focusGetCurrent $ s ^. focus
    case ev of
        (AppEvent Tick) -> do
            case currentFocus of
                Just (TaskList timer) -> do
                    case timer of
                        Pomodoro -> do
                            handleTimerTick s (timers . pomodoroState . timerCurrentValue) "Pomodoro round ended!" timer (if (s ^. pomodoroCyclesCounter) == 3 then TaskList LongBreak else TaskList ShortBreak) $ do
                                pomodoroCounter .= (s ^. pomodoroCounter) + 1
                                pomodoroCyclesCounter .= (s ^. pomodoroCyclesCounter) + 1
                        ShortBreak -> do
                            handleTimerTick s (timers . shortBreakState . timerCurrentValue) "Short break ended!" timer (TaskList Pomodoro) (pure ())
                        LongBreak -> do
                            handleTimerTick s (timers . longBreakState . timerCurrentValue) "Long break ended!" timer (TaskList Pomodoro) $ do
                                pomodoroCyclesCounter .= 0
                _ -> return ()
        (VtyEvent vev@(EvKey k ms)) -> do
            let selectedListTask = listSelectedElement (s ^. taskList)
                taskEditorContent = unlines $ getEditContents (s ^. taskEditor)
                tasksFilePath = configFilePathValue $ s ^. (configFile . tasksFilePathSetting)
                currentTasks = toList $ listElements (s ^. taskList)
                ( selectedIndex
                    , selectedTask
                    ) = case selectedListTask of
                        Just (idx, task) -> (idx, task)
                        _ -> (-1, mkTask "" $ Just False)
            case currentFocus of
                Just (TaskEdit action) -> do
                    case (k, ms) of
                        (KIns, []) -> saveTask currentTasks tasksFilePath taskEditorContent selectedTask action s
                        (KEsc, []) -> do
                            taskEditor .= editor (TaskEdit action) (Just 5) ""
                            changeFocus (TaskList Pomodoro) s
                        _ -> do
                            zoom taskEditor $ handleEditorEvent ev
                Just (TaskList timer) -> do
                    case (k, ms) of
                        (KChar 'e', []) -> do
                            let selectedTaskContent = selectedTask ^. taskContent
                            taskEditor .= editor (TaskEdit Edit) (Just 5) selectedTaskContent
                            changeFocus (TaskEdit Edit) s
                        (KChar 'c', [MCtrl]) -> do
                            updatedTaskList <- liftIO $ writeTasks tasksFilePath $ updateTaskList currentTasks (ChangeTaskCompletion selectedTask)
                            taskList .= listReplace (fromList updatedTaskList) (listSelected $ s ^. taskList) (s ^. taskList)
                        (KDel, []) -> do
                            updatedTaskList <- liftIO $ writeTasks tasksFilePath $ updateTaskList currentTasks (DeleteTask selectedTask)
                            if selectedIndex - 1 == length updatedTaskList
                                then taskList .= listReplace (fromList updatedTaskList) (Just selectedIndex) (s ^. taskList)
                                else
                                    if selectedIndex == 0
                                        then taskList .= listReplace (fromList updatedTaskList) (Just 0) (s ^. taskList)
                                        else taskList .= listReplace (fromList updatedTaskList) (Just $ length updatedTaskList - 1) (s ^. taskList)
                        (KChar 't', []) -> do
                            changeFocus (TaskEdit Insert) s
                        (KChar 'q', []) -> do
                            halt
                        (KChar 's', []) -> do
                            when ((s ^. timerStartStopSoundVolume) > 0) $ do
                                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) TimerStartStop (s ^. timerStartStopSoundVolume)
                                timerRunning .= not (s ^. timerRunning)
                        (KChar 'r', []) -> do
                            resetTimer s timer
                        (KBackTab, []) -> do
                            timerRunning .= False
                            case timer of
                                Pomodoro -> changeFocus (TaskList ShortBreak) s
                                ShortBreak -> changeFocus (TaskList LongBreak) s
                                LongBreak -> changeFocus (TaskList Pomodoro) s
                        (KChar 'p', []) -> changeFocus Config s
                        _ -> zoom taskList $ handleListEventVi handleListEvent vev
                Just Config -> do
                    case (k, ms) of
                        (KChar 'q', []) -> changeFocus (TaskList Pomodoro) s
                        (KEsc, []) -> changeFocus (TaskList Pomodoro) s
                        (KEnter, []) -> do
                            let selectedConfigElement = listSelectedElement (s ^. configList)
                                (_, selectedConfigSetting) = case selectedConfigElement of
                                    Just (idx, configSetting) -> (idx, configSetting)
                                    _ -> (-1, ConfigSetting{_configLabel = "", _configValue = ConfigInitialTimer Pomodoro 0})
                            case selectedConfigSetting ^. configValue of
                                ConfigTimerPopupAlert _ -> do
                                    handleConfigUpdate s timerPopupAlertSetting (ConfigTimerPopupAlert $ not (s ^. timerPopupAlert)) configList configFile
                                ConfigInitialTimer timer _ -> do
                                    initialTimerConfigDialog .= initialTimerDialog (Just 0) timer
                                    changeFocus (InitialTimerDialog timer) s
                                ConfigTasksFilePath _ -> do
                                    changeFocus TasksFilePathBrowser s
                                ConfigTimerAlertSoundVolume _ -> do
                                    timerAlertSoundVolumeConfigDialog .= soundVolumeDialog (Just "Timer alert sound volume") (Just (s ^. timerAlertSoundVolume))
                                    changeFocus TimerAlertSoundVolumeDialog s
                                ConfigTimerTickSoundVolume _ -> do
                                    timerAlertSoundVolumeConfigDialog .= soundVolumeDialog (Just "Timer tick sound volume") (Just (s ^. timerTickSoundVolume))
                                    changeFocus TimerTickSoundVolumeDialog s
                                ConfigTimerStartStopSoundVolume _ -> do
                                    timerStartStopSoundVolumeConfigDialog .= soundVolumeDialog (Just "Timer start/stop sound volume") (Just (s ^. timerStartStopSoundVolume))
                                    changeFocus TimerStartStopSoundVolumeDialog s
                                ConfigAudioDirectoryPath _ -> do
                                    changeFocus AudioDirectoryPathBrowser s
                        _ -> zoom configList $ handleListEventVi handleListEvent vev
                Just (InitialTimerDialog timer) -> do
                    case (k, ms) of
                        (KUp, []) -> do
                            timerState timer . timerInitialValue .= (s ^. (timerState timer . timerInitialValue)) + 60
                        (KDown, []) -> do
                            timerState timer . timerInitialValue .= max ((s ^. (timerState timer . timerInitialValue)) - 60) 0
                        (KEnter, []) -> case dialogSelection (s ^. initialTimerConfigDialog) of
                            Just SaveInitialTimer -> do
                                handleConfigUpdate s (initialTimerSettingL timer) (ConfigInitialTimer timer (s ^. (timerState timer . timerInitialValue))) configList configFile
                                changeFocus Config s
                            Just CloseInitialTimerDialog -> do
                                resetTimer s timer
                                changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> do
                            resetTimer s timer
                            changeFocus Config s
                        (KEsc, []) -> do
                            resetTimer s timer
                            changeFocus Config s
                        _ -> zoom initialTimerConfigDialog $ handleDialogEvent vev
                Just TasksFilePathBrowser -> do
                    if not $ fileBrowserIsSearching (s ^. tasksFilePathBrowser)
                        then do
                            case (k, ms) of
                                (KEsc, []) -> changeFocus Config s
                                (KChar 'q', []) -> changeFocus Config s
                                (KEnter, []) -> do
                                    case fileBrowserCursor (s ^. tasksFilePathBrowser) of
                                        Just fileInfo -> do
                                            if fileType fileInfo == Just RegularFile
                                                then do
                                                    let updatedTasksFilePath = fileInfoFilePath fileInfo
                                                    handleConfigUpdate s tasksFilePathSetting (ConfigTasksFilePath updatedTasksFilePath) configList configFile
                                                    newTaskFile <- liftIO $ readTasks updatedTasksFilePath
                                                    taskList .= listReplace (fromList newTaskFile) (listSelected $ s ^. taskList) (s ^. taskList)
                                                    changeFocus Config s
                                                else zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                                        Nothing -> zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                                    return ()
                                _ -> zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                        else zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                Just AudioDirectoryPathBrowser -> do
                    if not $ fileBrowserIsSearching (s ^. audioDirectoryPathBrowser)
                        then do
                            case (k, ms) of
                                (KEsc, []) -> changeFocus Config s
                                (KChar 'q', []) -> changeFocus Config s
                                (KChar 'C', []) -> do
                                    let updatedAudioDirectoryPath = getWorkingDirectory (s ^. audioDirectoryPathBrowser)
                                    handleConfigUpdate s audioDirectoryPathSetting (ConfigAudioDirectoryPath updatedAudioDirectoryPath) configList configFile
                                    audioDirectoryPath .= updatedAudioDirectoryPath
                                    changeFocus Config s
                                (KChar 'c', []) -> do
                                    case fileBrowserCursor (s ^. audioDirectoryPathBrowser) of
                                        Just fileInfo -> do
                                            if fileType fileInfo == Just Directory
                                                then do
                                                    let updatedAudioDirectoryPath = fileInfoFilePath fileInfo
                                                    handleConfigUpdate s audioDirectoryPathSetting (ConfigAudioDirectoryPath updatedAudioDirectoryPath) configList configFile
                                                    audioDirectoryPath .= updatedAudioDirectoryPath
                                                    changeFocus Config s
                                                else zoom audioDirectoryPathBrowser $ handleFileBrowserEvent vev
                                        Nothing -> zoom audioDirectoryPathBrowser $ handleFileBrowserEvent vev
                                    return ()
                                _ -> zoom audioDirectoryPathBrowser $ handleFileBrowserEvent vev
                        else zoom audioDirectoryPathBrowser $ handleFileBrowserEvent vev
                Just TimerAlertSoundVolumeDialog -> do
                    case (k, ms) of
                        (KUp, []) -> do
                            timerAlertSoundVolume .= min (max ((s ^. timerAlertSoundVolume) + 5) 0) 100
                        (KDown, []) -> do
                            timerAlertSoundVolume .= min (max ((s ^. timerAlertSoundVolume) - 5) 0) 100
                        (KEnter, []) -> case dialogSelection (s ^. timerAlertSoundVolumeConfigDialog) of
                            Just SaveSoundVolume -> do
                                handleConfigUpdate s timerAlertSoundVolumeSetting (ConfigTimerAlertSoundVolume (s ^. timerAlertSoundVolume)) configList configFile
                            Just PlayTestAudio -> do
                                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) TimerEnded (s ^. timerAlertSoundVolume)
                                return ()
                            Just CloseSoundVolumeDialog -> do
                                timerAlertSoundVolume .= configIntValue (s ^. configFile . timerAlertSoundVolumeSetting)
                                changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> do
                            timerAlertSoundVolume .= configIntValue (s ^. configFile . timerAlertSoundVolumeSetting)
                            changeFocus Config s
                        (KEsc, []) -> do
                            timerAlertSoundVolume .= configIntValue (s ^. configFile . timerAlertSoundVolumeSetting)
                            changeFocus Config s
                        _ -> zoom timerAlertSoundVolumeConfigDialog $ handleDialogEvent vev
                Just TimerTickSoundVolumeDialog -> do
                    case (k, ms) of
                        (KUp, []) ->
                            timerTickSoundVolume .= min (max ((s ^. timerTickSoundVolume) + 5) 0) 100
                        (KDown, []) ->
                            timerTickSoundVolume .= min (max ((s ^. timerTickSoundVolume) - 5) 0) 100
                        (KEnter, []) -> case dialogSelection (s ^. timerTickSoundVolumeConfigDialog) of
                            Just SaveSoundVolume -> do
                                handleConfigUpdate s timerTickSoundVolumeSetting (ConfigTimerTickSoundVolume (s ^. timerTickSoundVolume)) configList configFile
                            Just PlayTestAudio -> do
                                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) TimerTick (s ^. timerTickSoundVolume)
                                return ()
                            Just CloseSoundVolumeDialog -> do
                                timerTickSoundVolume .= configIntValue (s ^. configFile . timerTickSoundVolumeSetting)
                                changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> do
                            timerTickSoundVolume .= configIntValue (s ^. configFile . timerTickSoundVolumeSetting)
                            changeFocus Config s
                        (KEsc, []) -> do
                            timerTickSoundVolume .= configIntValue (s ^. configFile . timerTickSoundVolumeSetting)
                            changeFocus Config s
                        _ -> zoom timerTickSoundVolumeConfigDialog $ handleDialogEvent vev
                Just TimerStartStopSoundVolumeDialog -> do
                    case (k, ms) of
                        (KUp, []) ->
                            timerStartStopSoundVolume .= min (max ((s ^. timerStartStopSoundVolume) + 5) 0) 100
                        (KDown, []) -> do
                            timerStartStopSoundVolume .= min (max ((s ^. timerStartStopSoundVolume) - 5) 0) 100
                        (KEnter, []) -> case dialogSelection (s ^. timerStartStopSoundVolumeConfigDialog) of
                            Just SaveSoundVolume -> do
                                handleConfigUpdate s timerStartStopSoundVolumeSetting (ConfigTimerStartStopSoundVolume (s ^. timerStartStopSoundVolume)) configList configFile
                            Just PlayTestAudio -> do
                                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) TimerStartStop (s ^. timerStartStopSoundVolume)
                                return ()
                            Just CloseSoundVolumeDialog -> do
                                timerStartStopSoundVolume .= configIntValue (s ^. configFile . timerStartStopSoundVolumeSetting)
                                changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> do
                            timerStartStopSoundVolume .= configIntValue (s ^. configFile . timerStartStopSoundVolumeSetting)
                            changeFocus Config s
                        (KEsc, []) -> do
                            timerStartStopSoundVolume .= configIntValue (s ^. configFile . timerStartStopSoundVolumeSetting)
                            changeFocus Config s
                        _ -> zoom timerStartStopSoundVolumeConfigDialog $ handleDialogEvent vev
                _ -> return ()
        _ -> return ()

timerState :: Timer -> Lens' AppState TimerState
timerState timer = case timer of
    Pomodoro -> timers . pomodoroState
    ShortBreak -> timers . shortBreakState
    LongBreak -> timers . longBreakState

resetTimer :: AppState -> Timer -> EventM Name AppState ()
resetTimer s timer = timerState timer . timerInitialValue .= configIntValue (s ^. (configFile . initialTimerSettingL timer))

handleTimerTick ::
    AppState ->
    Lens' AppState Int ->
    String ->
    Timer ->
    Name ->
    EventM Name AppState () ->
    EventM Name AppState ()
handleTimerTick s timerL popupText timer nextFocus f = do
    let currentAlertSoundVolume = s ^. timerAlertSoundVolume
    when (s ^. timerRunning) $ do
        tickTimer (s ^. audioProvider) (s ^. audioDirectoryPath) timerL (s ^. timerL) (s ^. timerTickSoundVolume)
        when (s ^. timerL == 0) $ do
            stopTimer
            when (currentAlertSoundVolume > 0) $ do
                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) TimerEnded currentAlertSoundVolume
                return ()
            alertRoundEnded (s ^. notificationProvider) popupText $ configBoolValue $ s ^. (configFile . timerPopupAlertSetting)
            resetTimer s timer
            f
            changeFocus nextFocus s
  where
    alertRoundEnded sNotificationProvider alertMsg timerEndedNotificationIsActive
        | timerEndedNotificationIsActive = do
            liftIO $
                maybe
                    (return ())
                    ( \jnftp -> do
                        let args = case jnftp of
                                NotifySend -> "-a homodoro \"" <> alertMsg <> "\""
                                Zenity -> "--title=homodoro --text=" <> alertMsg
                        callNotificationProvider jnftp args
                    )
                    sNotificationProvider
        | otherwise = return ()

handleConfigUpdate ::
    AppState ->
    Lens' ConfigFile ConfigSetting ->
    ConfigSettingValue ->
    Lens' AppState (List Name ConfigSetting) ->
    Lens' AppState ConfigFile ->
    EventM Name AppState ()
handleConfigUpdate s settingL newSettingValue configListL configFileL = do
    updatedConfigFile <- liftIO $ updateConfig (s ^. configFileL) settingL newSettingValue
    configListL .= listReplace (fromList $ configFileSettings updatedConfigFile) (listSelected $ s ^. configList) (s ^. configList)
    configFileL .= updatedConfigFile

fileType :: FileInfo -> Maybe FileType
fileType fileInfo = case fileInfoFileStatus fileInfo of
    Left _ -> Nothing
    Right fileStatus -> fileStatusFileType fileStatus

tickTimer :: Maybe AudioProvider -> FilePath -> Lens' AppState Int -> Int -> Int -> EventM Name AppState ()
tickTimer sAudioProvider sAudioDirectoryPath timerL timerValue tickVolume = do
    when (tickVolume > 0 && timerValue > 0) $ do
        _ <- liftIO $ forkIO $ playAudio sAudioProvider sAudioDirectoryPath TimerTick tickVolume
        return ()
    timerL .= max (timerValue - 1) 0

stopTimer :: EventM Name AppState ()
stopTimer = timerRunning .= False

saveTask :: [Task] -> FilePath -> Text -> Task -> TaskAction -> AppState -> EventM Name AppState ()
saveTask tasks fp taskEditorContent selectedTask action s = do
    if not (null taskEditorContent) && not (taskExists tasks taskEditorContent)
        then do
            let taskOperation = case action of
                    Insert -> AppendTask $ mkTask taskEditorContent $ Just False
                    Edit -> EditTask selectedTask taskEditorContent
            updatedTasks <- liftIO $ writeTasks fp $ updateTaskList tasks taskOperation
            taskList .= listReplace (fromList updatedTasks) (listSelected $ s ^. taskList) (s ^. taskList)
            clearTaskEditor action
            changeFocus (TaskList Pomodoro) s
        else do
            clearTaskEditor action
            changeFocus (TaskList Pomodoro) s

clearTaskEditor :: TaskAction -> EventM Name AppState ()
clearTaskEditor ta = taskEditor .= editor (TaskEdit ta) (Just 5) ""

playAudio :: Maybe AudioProvider -> FilePath -> Audio -> Int -> IO ()
playAudio ap currentAudioDirectoryPath audio vol = do
    filesInDir <- listDirectory currentAudioDirectoryPath
    case find (\file -> map toLower (takeBaseName file) == map toLower (show audio)) filesInDir of
        Just audioFileName ->
            maybe
                (return ())
                ( \jap -> do
                    let args = case jap of
                            MPV -> currentAudioDirectoryPath </> audioFileName <> " --video=no --volume=" <> show vol
                            FFPlay -> "-vn -nodisp " <> currentAudioDirectoryPath </> audioFileName
                    callAudioProvider jap args
                )
                ap
        Nothing -> return ()
