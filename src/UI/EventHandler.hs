{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI.EventHandler (handleEvent) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt, zoom)
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Dialog (dialogSelection, handleDialogEvent)
import Brick.Widgets.Edit (editor, getEditContents, handleEditorEvent)
import Brick.Widgets.FileBrowser (FileInfo (fileInfoFilePath, fileInfoFileStatus), FileStatus (fileStatusFileType), FileType (Directory, RegularFile), fileBrowserCursor, fileBrowserIsSearching, getWorkingDirectory, handleFileBrowserEvent)
import Brick.Widgets.List (
    GenericList (listSelected),
    List,
    handleListEvent,
    handleListEventVi,
    listReplace,
    listSelectedElement,
 )
import Config (configFilePathValue, configFileSettings, findConfigSetting, maybeConfigIntValue, readAlertSoundVolume, readInitialTimer, readStartStopSound, readTimerPopupAlert, updateConfig)
import Control.Concurrent (forkIO)
import Control.Lens (Lens', (.=), (^.))
import Control.Monad.State (MonadIO (liftIO), MonadState (..), when)
import Data.Char (toLower)
import Data.List (find)
import Data.Text (Text, null, unlines)
import Data.Vector (fromList)
import Graphics.Vty (Event (EvKey), Key (..), Modifier (..))
import Process (callAudioProvider, callNotificationProvider)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Task (mkTask, readTasks, taskExists, updateTaskList)
import Types (AppState, Audio (..), AudioProvider (FFPlay, MPV), ConfigFile, ConfigFileOperation (..), ConfigSetting (ConfigSetting, _configLabel, _configValue), ConfigSettingValue (..), InitialTimerDialogChoice (..), Name (..), NotificationProvider (NotifySend, Zenity), SoundVolumeDialogChoice (CloseSoundVolumeDialog, PlayTestAudio), Task, TaskAction (Edit, Insert), TaskListOperation (AppendTask, ChangeTaskCompletion, DeleteTask, EditTask), Tick (Tick), Timer (LongBreak, Pomodoro, ShortBreak), audioDirectoryPath, audioDirectoryPathBrowser, audioDirectoryPathSetting, audioProvider, configList, configValue, focus, initialTimerConfigDialog, longBreakTimer, notificationProvider, pomodoroCounter, pomodoroCyclesCounter, pomodoroTimer, shortBreakTimer, taskContent, taskEditor, taskList, tasksFilePathBrowser, tasksFilePathSetting, timerAlertSoundVolume, timerAlertSoundVolumeConfigDialog, timerAlertSoundVolumeSetting, timerRunning, timerTickSoundVolume, timerTickSoundVolumeConfigDialog, timerTickSoundVolumeSetting)
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
                            handleTimerTick s pomodoroTimer "Pomodoro round ended!" timer (if (s ^. pomodoroCyclesCounter) == 3 then TaskList LongBreak else TaskList ShortBreak) $ do
                                pomodoroCounter .= (s ^. pomodoroCounter) + 1
                                pomodoroCyclesCounter .= (s ^. pomodoroCyclesCounter) + 1
                        ShortBreak -> do
                            handleTimerTick s shortBreakTimer "Short break ended!" timer (TaskList Pomodoro) (pure ())
                        LongBreak -> do
                            handleTimerTick s longBreakTimer "Long break ended!" timer (TaskList Pomodoro) $ do
                                pomodoroCyclesCounter .= 0
                _ -> return ()
        (VtyEvent vev@(EvKey k ms)) -> do
            let selectedListTask = listSelectedElement (s ^. taskList)
                taskEditorContent = unlines $ getEditContents (s ^. taskEditor)
                (selectedIndex, selectedTask) = case selectedListTask of
                    Just (idx, task) -> (idx, task)
                    _ -> (-1, mkTask "" $ Just False)
            case currentFocus of
                Just (TaskEdit action) -> do
                    case (k, ms) of
                        (KIns, []) -> saveTask taskEditorContent selectedTask action s
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
                            modifiedTaskList <- liftIO $ updateTaskList (ChangeTaskCompletion selectedTask)
                            taskList .= listReplace (fromList modifiedTaskList) (listSelected $ s ^. taskList) (s ^. taskList)
                        (KDel, []) -> do
                            modifiedTaskList <- liftIO $ updateTaskList (DeleteTask selectedTask)
                            if selectedIndex - 1 == length modifiedTaskList
                                then taskList .= listReplace (fromList modifiedTaskList) (Just selectedIndex) (s ^. taskList)
                                else
                                    if selectedIndex == 0
                                        then taskList .= listReplace (fromList modifiedTaskList) (Just 0) (s ^. taskList)
                                        else taskList .= listReplace (fromList modifiedTaskList) (Just $ length modifiedTaskList - 1) (s ^. taskList)
                        (KChar 't', []) -> do
                            changeFocus (TaskEdit Insert) s
                        (KChar 'q', []) -> do
                            halt
                        (KChar 's', []) -> do
                            startStopSoundActive <- liftIO readStartStopSound
                            currentSoundVolumeConfig <- liftIO readAlertSoundVolume
                            when startStopSoundActive $ do
                                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) (if s ^. timerRunning then TimerStop else TimerStart) currentSoundVolumeConfig
                                return ()
                            timerRunning .= not (s ^. timerRunning)
                        (KChar 'r', []) -> do
                            initialTimer <- liftIO $ readInitialTimer timer
                            case timer of
                                Pomodoro -> do
                                    pomodoroTimer .= initialTimer
                                ShortBreak -> do
                                    shortBreakTimer .= initialTimer
                                LongBreak -> do
                                    longBreakTimer .= initialTimer
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
                                ConfigTimerStartStopSound _ -> do
                                    handleConfigUpdate ToggleStartStopSound s configList (\_ -> pure ())
                                ConfigTimerPopupAlert _ -> do
                                    handleConfigUpdate ToggleTimerPopupAlert s configList (\_ -> pure ())
                                ConfigInitialTimer timer _ -> do
                                    initialTimerConfigDialog .= initialTimerDialog (Just 0) timer
                                    changeFocus (InitialTimerDialog timer) s
                                ConfigTasksFilePath _ -> do
                                    changeFocus TasksFilePathBrowser s
                                ConfigTimerAlertSoundVolume _ -> do
                                    currentSoundVolumeConfig <- liftIO readAlertSoundVolume
                                    timerAlertSoundVolumeConfigDialog .= soundVolumeDialog (Just "Timer alert sound volume") (Just currentSoundVolumeConfig)
                                    changeFocus TimerAlertSoundVolumeDialog s
                                ConfigTimerTickSoundVolume _ -> do
                                    currentSoundVolumeConfig <- liftIO readAlertSoundVolume
                                    timerAlertSoundVolumeConfigDialog .= soundVolumeDialog (Just "Timer tick sound volume") (Just currentSoundVolumeConfig)
                                    changeFocus TimerTickSoundVolumeDialog s
                                ConfigAudioDirectoryPath _ -> do
                                    changeFocus AudioDirectoryPathBrowser s
                        _ -> zoom configList $ handleListEventVi handleListEvent vev
                Just (InitialTimerDialog timer) -> do
                    case (k, ms) of
                        (KUp, []) -> do
                            handleConfigUpdate (AddInitialTimer timer 60) s configList (\_ -> pure ())
                        (KDown, []) -> do
                            handleConfigUpdate (AddInitialTimer timer (-60)) s configList (\_ -> pure ())
                        (KEnter, []) -> case dialogSelection (s ^. initialTimerConfigDialog) of
                            Just CloseInitialTimerDialog -> changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> changeFocus Config s
                        (KEsc, []) -> changeFocus Config s
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
                                                    handleConfigUpdate (SetFilePathSetting tasksFilePathSetting (ConfigTasksFilePath $ fileInfoFilePath fileInfo)) s configList $
                                                        \_ -> do
                                                            changeFocus Config s
                                                            newTaskFile <- liftIO readTasks
                                                            taskList .= listReplace (fromList newTaskFile) (listSelected $ s ^. taskList) (s ^. taskList)
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
                                    handleConfigUpdate (SetFilePathSetting audioDirectoryPathSetting (ConfigAudioDirectoryPath $ getWorkingDirectory (s ^. audioDirectoryPathBrowser))) s configList $
                                        \updatedConfigFile -> do
                                            changeFocus Config s
                                            audioDirectoryPath .= configFilePathValue (updatedConfigFile ^. audioDirectoryPathSetting)
                                (KChar 'c', []) -> do
                                    case fileBrowserCursor (s ^. audioDirectoryPathBrowser) of
                                        Just fileInfo -> do
                                            if fileType fileInfo == Just Directory
                                                then do
                                                    handleConfigUpdate (SetFilePathSetting audioDirectoryPathSetting (ConfigAudioDirectoryPath $ fileInfoFilePath fileInfo)) s configList $
                                                        \updatedConfigFile -> do
                                                            changeFocus Config s
                                                            audioDirectoryPath .= configFilePathValue (updatedConfigFile ^. audioDirectoryPathSetting)
                                                else zoom audioDirectoryPathBrowser $ handleFileBrowserEvent vev
                                        Nothing -> zoom audioDirectoryPathBrowser $ handleFileBrowserEvent vev
                                    return ()
                                _ -> zoom audioDirectoryPathBrowser $ handleFileBrowserEvent vev
                        else zoom audioDirectoryPathBrowser $ handleFileBrowserEvent vev
                Just TimerAlertSoundVolumeDialog -> do
                    case (k, ms) of
                        (KUp, []) -> do
                            handleConfigUpdate (AddSoundVolume timerAlertSoundVolumeSetting 5) s configList $
                                \updatedConfigSettings -> do
                                    timerAlertSoundVolume .= maybeConfigIntValue (findConfigSetting (ConfigTimerAlertSoundVolume 0) (configFileSettings updatedConfigSettings))
                        (KDown, []) -> do
                            handleConfigUpdate (AddSoundVolume timerAlertSoundVolumeSetting $ -5) s configList $
                                \updatedConfigSettings -> do
                                    timerAlertSoundVolume .= maybeConfigIntValue (findConfigSetting (ConfigTimerAlertSoundVolume 0) (configFileSettings updatedConfigSettings))
                        (KEnter, []) -> case dialogSelection (s ^. timerAlertSoundVolumeConfigDialog) of
                            Just PlayTestAudio -> do
                                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) TimerEnded (s ^. timerAlertSoundVolume)
                                return ()
                            Just CloseSoundVolumeDialog -> changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> changeFocus Config s
                        (KEsc, []) -> changeFocus Config s
                        _ -> zoom timerAlertSoundVolumeConfigDialog $ handleDialogEvent vev
                Just TimerTickSoundVolumeDialog -> do
                    case (k, ms) of
                        (KUp, []) ->
                            handleConfigUpdate (AddSoundVolume timerTickSoundVolumeSetting 5) s configList $
                                \updatedConfigSettings -> do
                                    timerTickSoundVolume .= maybeConfigIntValue (findConfigSetting (ConfigTimerAlertSoundVolume 0) (configFileSettings updatedConfigSettings))
                        (KDown, []) -> do
                            handleConfigUpdate (AddSoundVolume timerTickSoundVolumeSetting $ -5) s configList $
                                \updatedConfigSettings -> do
                                    timerTickSoundVolume .= maybeConfigIntValue (findConfigSetting (ConfigTimerTickSoundVolume 0) (configFileSettings updatedConfigSettings))
                        (KEnter, []) -> case dialogSelection (s ^. timerTickSoundVolumeConfigDialog) of
                            Just PlayTestAudio -> do
                                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) TimerTick (s ^. timerTickSoundVolume)
                                return ()
                            Just CloseSoundVolumeDialog -> changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> changeFocus Config s
                        (KEsc, []) -> changeFocus Config s
                        _ -> zoom timerTickSoundVolumeConfigDialog $ handleDialogEvent vev
                _ -> return ()
        _ -> return ()

handleTimerTick ::
    AppState ->
    Lens' AppState Int ->
    String ->
    Timer ->
    Name ->
    EventM Name AppState () ->
    EventM Name AppState ()
handleTimerTick s timerL popupText timer nextFocus afterTickF = do
    when (s ^. timerRunning) $ do
        tickTimer (s ^. audioProvider) (s ^. audioDirectoryPath) timerL (s ^. timerL) (s ^. timerTickSoundVolume)
        when (s ^. timerL == 0) $ do
            stopTimer
            let currentAlertSoundVolume = s ^. timerAlertSoundVolume
            when (currentAlertSoundVolume > 0) $ do
                _ <- liftIO $ forkIO $ playAudio (s ^. audioProvider) (s ^. audioDirectoryPath) TimerEnded currentAlertSoundVolume
                return ()
            timerEndedPopupAlertIsActive <- liftIO readTimerPopupAlert
            alertRoundEnded (s ^. notificationProvider) popupText timerEndedPopupAlertIsActive
            resetTimer timerL timer
            afterTickF
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
    ConfigFileOperation ->
    AppState ->
    Lens' AppState (List Name ConfigSetting) ->
    (ConfigFile -> EventM Name AppState ()) ->
    EventM Name AppState ()
handleConfigUpdate cfgOp s configListL afterF = do
    updatedConfigSettings <- liftIO $ updateConfig cfgOp
    configListL .= listReplace (fromList $ configFileSettings updatedConfigSettings) (listSelected $ s ^. configList) (s ^. configList)
    afterF updatedConfigSettings

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

resetTimer :: Lens' AppState Int -> Timer -> EventM Name AppState ()
resetTimer timerL timer = do
    initialTimer <- liftIO $ readInitialTimer timer
    timerL .= initialTimer

saveTask :: Text -> Task -> TaskAction -> AppState -> EventM Name AppState ()
saveTask taskEditorContent selectedTask action s = do
    taskAlreadyExists <- liftIO $ taskExists taskEditorContent
    if not (null taskEditorContent) && not taskAlreadyExists
        then do
            let taskOperation = case action of
                    Insert -> AppendTask $ mkTask taskEditorContent $ Just False
                    Edit -> EditTask selectedTask taskEditorContent
            updatedTasks <- liftIO $ updateTaskList taskOperation
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
                            MPV -> currentAudioDirectoryPath </> audioFileName <> " --volume=" <> show vol
                            FFPlay -> ""
                    callAudioProvider jap args
                )
                ap
        Nothing -> return ()
