{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI.EventHandler (handleEvent) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt, zoom)
import Brick.Widgets.Dialog (dialogSelection, handleDialogEvent)
import Brick.Widgets.Edit (editor, getEditContents, handleEditorEvent)
import Config (configFileSettings, readInitialTimer, updateConfig, readTimerPopupAlert, readStartStopSound, readAlertSoundVolume, findConfigSetting, maybeConfigIntValue)
import Control.Concurrent (forkIO)
import Control.Lens ((.=), (^.), Lens', uses)
import Control.Monad.State (MonadIO (liftIO), MonadState (..), when)
import Notify ( alertRoundEnded, playAudio )
import Resources (AppState, Audio (..), ConfigFileOperation (..), ConfigSetting (ConfigSetting, _configLabel, _configValue), ConfigSettingValue (..), InitialTimerDialogChoice (..), Name (..), TaskAction (Edit, Insert), TaskListOperation (AppendTask, ChangeTaskCompletion, DeleteTask, EditTask), Tick (Tick), Timer (LongBreak, Pomodoro, ShortBreak), configList, configValue, focus, longBreakTimer, pomodoroTimer, shortBreakTimer, taskContent, taskEditor, taskList, timerRunning, pomodoroCounter, pomodoroCyclesCounter, tasksFilePathBrowser, initialTimerConfigDialog, Task, SoundVolumeDialogChoice (CloseSoundVolumeDialog, PlayTestAudio), alertSoundVolumeConfigDialog, currentAlertSoundVolume, timerAlertSoundVolume, timerTickSoundVolumeConfigDialog, currentTimerTickSoundVolume, timerTickSoundVolume, ConfigFile)
import Task (mkTask, taskExists, updateTaskList)
import UI.Util (changeFocus)
import Brick.Widgets.FileBrowser (handleFileBrowserEvent, fileBrowserIsSearching, fileBrowserCursor, FileInfo (fileInfoFilePath, fileInfoFileStatus), FileType (RegularFile), FileStatus (fileStatusFileType))
import UI.Config ( initialTimerDialog, soundVolumeDialog )
import Brick.Widgets.List
    ( handleListEventVi, listReplace, listSelectedElement, GenericList (listSelected), handleListEvent )
import Data.Vector (fromList)
import Graphics.Vty (Event(EvKey), Key (..), Modifier (..))
import Brick.Focus (focusGetCurrent)

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
                            handleTimerTick s pomodoroTimer (s ^. currentTimerTickSoundVolume) "Pomodoro round ended!" timer $ do
                                pomodoroCounter .= (s ^. pomodoroCounter) + 1
                                pomodoroCyclesCounter .= (s ^. pomodoroCyclesCounter) + 1
                                updatedCycleCounter <- uses pomodoroCyclesCounter id
                                if updatedCycleCounter == 4
                                    then do
                                        changeFocus (TaskList LongBreak) s
                                    else changeFocus (TaskList ShortBreak) s
                        ShortBreak -> do
                            handleTimerTick s shortBreakTimer (s ^. currentTimerTickSoundVolume) "Short break ended!" timer $ do
                                changeFocus (TaskList Pomodoro) s
                        LongBreak -> do
                            handleTimerTick s longBreakTimer (s ^. currentTimerTickSoundVolume) "Long break ended!" timer $ do
                                changeFocus (TaskList Pomodoro) s
                                pomodoroCyclesCounter .= 0
                _ -> return ()
        (VtyEvent vev@(EvKey k ms)) -> do
            let selectedListTask = listSelectedElement (s ^. taskList)
                taskEditorContent = unlines $ getEditContents (s ^. taskEditor)
                (selectedIndex, selectedTask) = case selectedListTask of
                    Just (idx, task) -> (idx, task)
                    _ -> (-1, mkTask "")
            case currentFocus of
                Just (TaskEdit action) -> do
                    case (k, ms) of
                        (KIns, []) -> saveTask taskEditorContent selectedTask action s
                        (KEsc, []) -> do
                            taskEditor .= editor (TaskEdit Insert) (Just 5) ""
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
                                _ <- liftIO $ forkIO $ playAudio (if s ^. timerRunning then Stop else Start) currentSoundVolumeConfig
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
                                    handleConfigUpdate ToggleStartStopSound
                                        $ \updatedConfigSettings -> do
                                            updateConfigList s configList updatedConfigSettings
                                ConfigTimerPopupAlert _ -> do
                                    handleConfigUpdate ToggleTimerPopupAlert
                                        $ \updatedConfigSettings -> do
                                            updateConfigList s configList updatedConfigSettings
                                ConfigInitialTimer timer _ -> do
                                    initialTimerConfigDialog .= initialTimerDialog (Just 0) timer
                                    changeFocus (InitialTimerDialog timer) s
                                ConfigTasksFilePath _ -> do
                                    changeFocus TasksFilePathBrowser s
                                ConfigTimerAlertSoundVolume _ -> do
                                    currentSoundVolumeConfig <- liftIO readAlertSoundVolume
                                    alertSoundVolumeConfigDialog .= soundVolumeDialog (Just "Timer alert sound volume") (Just currentSoundVolumeConfig)
                                    changeFocus TimerAlertSoundVolumeDialog s
                                ConfigTimerTickSoundVolume _ -> do
                                    currentSoundVolumeConfig <- liftIO readAlertSoundVolume
                                    alertSoundVolumeConfigDialog .= soundVolumeDialog (Just "Timer tick sound volume") (Just currentSoundVolumeConfig)
                                    changeFocus TimerTickSoundVolumeDialog s
                        _ -> zoom configList $ handleListEventVi handleListEvent vev
                Just (InitialTimerDialog timer) -> do
                    case (k, ms) of
                        (KUp, []) -> do
                            handleConfigUpdate (AddInitialTimer timer 60)
                                $ \updatedConfigSettings -> do
                                    updateConfigList s configList updatedConfigSettings
                        (KDown, []) -> do
                            handleConfigUpdate (AddInitialTimer timer (-60))
                                $ \updatedConfigSettings -> do
                                    updateConfigList s configList updatedConfigSettings
                        (KEnter, []) -> case dialogSelection (s ^. initialTimerConfigDialog) of
                            Just CloseInitialTimerDialog -> changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> changeFocus Config s
                        (KEsc, []) -> changeFocus Config s
                        _ -> zoom initialTimerConfigDialog $ handleDialogEvent vev
                Just TasksFilePathBrowser -> do
                    if not $ fileBrowserIsSearching (s ^. tasksFilePathBrowser) then do
                        case (k, ms) of
                            (KEsc, []) -> changeFocus Config s
                            (KChar 'q', []) -> changeFocus Config s
                            (KEnter, []) -> do
                                case fileBrowserCursor (s ^. tasksFilePathBrowser) of
                                    Just fileInfo -> do
                                        if fileType fileInfo == Just RegularFile then do
                                            handleConfigUpdate (SetTasksFilePath (fileInfoFilePath fileInfo))
                                                $ \updatedConfigSettings -> do
                                                updateConfigList s configList updatedConfigSettings
                                                changeFocus Config s
                                        else
                                            zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                                    Nothing -> zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                                return ()
                            _ -> zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                    else zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                Just TimerAlertSoundVolumeDialog -> do
                    case (k, ms) of
                        (KUp, []) -> do
                            handleConfigUpdate (AddSoundVolume timerAlertSoundVolume 4)
                                $ \updatedConfigSettings -> do
                                    updateConfigList s configList updatedConfigSettings
                                    currentAlertSoundVolume .= maybeConfigIntValue (findConfigSetting (ConfigTimerAlertSoundVolume 0) (configFileSettings updatedConfigSettings))
                        (KDown, []) -> do
                            handleConfigUpdate (AddSoundVolume timerAlertSoundVolume $ -4)
                                $ \updatedConfigSettings -> do
                                    updateConfigList s configList updatedConfigSettings
                                    currentAlertSoundVolume .= maybeConfigIntValue (findConfigSetting (ConfigTimerAlertSoundVolume 0) (configFileSettings updatedConfigSettings))
                        (KEnter, []) -> case dialogSelection (s ^. alertSoundVolumeConfigDialog) of
                            Just PlayTestAudio -> do
                                _ <- liftIO $ forkIO $ playAudio TimerEnded (s ^. currentAlertSoundVolume)
                                return ()
                            Just CloseSoundVolumeDialog -> changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> changeFocus Config s
                        (KEsc, []) -> changeFocus Config s
                        _ -> zoom alertSoundVolumeConfigDialog $ handleDialogEvent vev
                Just TimerTickSoundVolumeDialog -> do
                    case (k, ms) of
                        (KUp, []) ->
                            handleConfigUpdate (AddSoundVolume timerTickSoundVolume 4)
                                $ \updatedConfigSettings -> do
                                    updateConfigList s configList updatedConfigSettings
                                    currentTimerTickSoundVolume .= maybeConfigIntValue (findConfigSetting (ConfigTimerAlertSoundVolume 0) (configFileSettings updatedConfigSettings))
                        (KDown, []) -> do
                            handleConfigUpdate (AddSoundVolume timerTickSoundVolume $ -4)
                                $ \updatedConfigSettings -> do
                                    updateConfigList s configList updatedConfigSettings
                                    currentTimerTickSoundVolume .= maybeConfigIntValue (findConfigSetting (ConfigTimerTickSoundVolume 0) (configFileSettings updatedConfigSettings))
                        (KEnter, []) -> case dialogSelection (s ^. timerTickSoundVolumeConfigDialog) of
                            Just PlayTestAudio -> do
                                _ <- liftIO $ forkIO $ playAudio TimerTick (s ^. currentTimerTickSoundVolume)
                                return ()
                            Just CloseSoundVolumeDialog -> changeFocus Config s
                            _ -> return ()
                        (KChar 'q', []) -> changeFocus Config s
                        (KEsc, []) -> changeFocus Config s
                        _ -> zoom timerTickSoundVolumeConfigDialog $ handleDialogEvent vev
                _ -> return ()
        _ -> return ()
        where updateConfigList s configListL updatedConfigSettings = do
                configListL .= listReplace (fromList $ configFileSettings updatedConfigSettings) (listSelected $ s ^. configList) (s ^. configList)

handleTimerTick :: AppState -> Lens' AppState Int -> Int -> String -> Timer -> EventM Name AppState () -> EventM Name AppState ()
handleTimerTick s timerL tickSoundVolume popupText timer afterTickF = do
    when (s ^. timerRunning) $ do
        tickTimer timerL (s ^. timerL) tickSoundVolume
        when (s ^. timerL == 0) $ do
            stopTimer
            playTimerEndedAudioAlert
            showTimerEndedPopupAlert popupText
            resetTimer timerL timer
            afterTickF

handleConfigUpdate :: ConfigFileOperation -> (ConfigFile -> EventM Name AppState ()) -> EventM Name AppState ()
handleConfigUpdate cfgOp afterF = do
    updatedConfigSettings <- liftIO $ updateConfig cfgOp
    afterF updatedConfigSettings

fileType :: FileInfo -> Maybe FileType
fileType fileInfo = case fileInfoFileStatus fileInfo of
    Left _ -> Nothing
    Right fileStatus -> fileStatusFileType fileStatus

tickTimer :: Lens' AppState Int -> Int -> Int -> EventM Name AppState ()
tickTimer timerL timerValue tickVolume = do 
    when (tickVolume > 0 && timerValue > 0) $ do
       _ <- liftIO $ forkIO $ playAudio TimerTick tickVolume
       return ()
    timerL .= max (timerValue - 1) 0

stopTimer :: EventM Name AppState ()
stopTimer = timerRunning .= False

playTimerEndedAudioAlert :: EventM Name AppState ()
playTimerEndedAudioAlert  = do
    currentSoundVolumeConfig <- liftIO readAlertSoundVolume
    when (currentSoundVolumeConfig > 0) $ do
        _ <- liftIO $ forkIO $ playAudio TimerEnded currentSoundVolumeConfig
        return ()

showTimerEndedPopupAlert :: String -> EventM Name AppState ()
showTimerEndedPopupAlert msg = do
    timerEndedPopupAlertIsActive <- liftIO readTimerPopupAlert
    when timerEndedPopupAlertIsActive $ do
        alertRoundEnded msg
        return ()

resetTimer :: Lens' AppState Int -> Timer -> EventM Name AppState ()
resetTimer timer currentTimer = do
    initialTimer <- liftIO $ readInitialTimer currentTimer
    timer .= initialTimer

saveTask :: String -> Task -> TaskAction -> AppState -> EventM Name AppState ()
saveTask taskEditorContent selectedTask action s = do
    taskAlreadyExists <- liftIO $ taskExists taskEditorContent
    if not (null taskEditorContent) && not taskAlreadyExists
        then do
            let taskOperation = case action of
                    Insert -> AppendTask $ mkTask taskEditorContent
                    Edit -> EditTask selectedTask taskEditorContent
            updatedTasks <- liftIO $ updateTaskList taskOperation
            taskList .= listReplace (fromList updatedTasks) (listSelected $ s ^. taskList) (s ^. taskList)
            taskEditor .= editor (TaskEdit action) (Just 5) ""
            changeFocus (TaskList Pomodoro) s
        else do
            taskEditor .= editor (TaskEdit action) (Just 5) ""
            changeFocus (TaskList Pomodoro) s