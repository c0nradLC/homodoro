{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI.EventHandler (handleEvent) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt)
import qualified Brick as BT
import qualified Brick.Focus as BF
import Brick.Widgets.Dialog (dialogSelection, handleDialogEvent)
import Brick.Widgets.Edit (editor, getEditContents)
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import Config (configFileSettings, readInitialTimer, updateConfig, readTimerNotificationAlert, readStartStopSound, readTimerSoundAlert)
import Control.Concurrent (forkIO)
import Control.Lens ((.=), (^.), Lens', uses)
import Control.Monad.State (MonadIO (liftIO), MonadState (..), when)
import qualified Data.Text as Txt
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import qualified Graphics.Vty as VE
import qualified Notify as NT
import Resources (AppState, Audio (..), ConfigFileOperation (..), ConfigSetting (ConfigSetting, _configLabel, _configValue), ConfigSettingValue (..), InitialTimerDialogChoice (..), Name (..), TaskAction (Edit, Insert), TaskListOperation (AppendTask, ChangeTaskCompletion, DeleteTask, EditTask), Tick (Tick), Timer (LongBreak, Pomodoro, ShortBreak), configList, configValue, focus, longBreakTimer, pomodoroTimer, shortBreakTimer, taskContent, taskEditor, taskList, timerRunning, pomodoroCounter, pomodoroCyclesCounter, tasksFilePathBrowser, initialTimerConfigDialog, Task)
import Task (mkTask, taskExists, updateTaskList)
import Util (changeFocus)
import qualified Config as CFG
import Brick.Widgets.FileBrowser (handleFileBrowserEvent, fileBrowserIsSearching, fileBrowserCursor, FileInfo (fileInfoFilePath, fileInfoFileStatus), FileType (RegularFile), FileStatus (fileStatusFileType))
import UI.Config

handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent ev = do
    s <- get
    let currentFocus = BF.focusGetCurrent $ s ^. focus
    case ev of
        (AppEvent Tick) -> do
            when (s ^. timerRunning) $ do
                case currentFocus of
                    Just (TaskList Pomodoro) -> do
                        tickTimer pomodoroTimer (s ^. pomodoroTimer)
                        when (s ^. pomodoroTimer == 0) $ do
                            stopTimer
                            timerEndedAudioAlert
                            timerEndedNotificationAlert "Pomodoro round ended!"
                            resetTimer pomodoroTimer Pomodoro
                            pomodoroCounter .= (s ^. pomodoroCounter) + 1
                            increasePomodoroCycleCounter s
                    Just (TaskList ShortBreak) -> do
                        tickTimer shortBreakTimer (s ^. shortBreakTimer)
                        when (s ^. shortBreakTimer == 0) $ do
                            stopTimer
                            timerEndedAudioAlert
                            timerEndedNotificationAlert "Short break ended!"
                            resetTimer shortBreakTimer ShortBreak
                            changeFocus (TaskList Pomodoro) s
                    Just (TaskList LongBreak) -> do
                        tickTimer longBreakTimer (s ^. longBreakTimer)
                        when (s ^. longBreakTimer == 0) $ do
                            stopTimer
                            timerEndedAudioAlert
                            timerEndedNotificationAlert "Long break ended!"
                            resetTimer longBreakTimer LongBreak
                            changeFocus (TaskList Pomodoro) s
                            pomodoroCyclesCounter .= 0
                    _ -> return ()
        (VtyEvent vev@(VE.EvKey k ms)) -> do
            let selectedListTask = BL.listSelectedElement (s ^. taskList)
                taskEditorContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
                (selectedIndex, selectedTask) = case selectedListTask of
                    Just (idx, task) -> (idx, task)
                    _ -> (-1, mkTask "")
            case currentFocus of
                Just (TaskEdit action) -> do
                    case (k, ms) of
                        (V.KIns, []) -> saveTask taskEditorContent selectedTask action s
                        (V.KEnter, [V.MCtrl]) -> saveTask taskEditorContent selectedTask action s
                        (V.KEsc, []) -> do
                            taskEditor .= editor (TaskEdit Insert) (Just 5) ""
                            changeFocus (TaskList Pomodoro) s
                        _ -> do
                            BT.zoom taskEditor $ BE.handleEditorEvent ev
                Just (TaskList timer) -> do
                    case (k, ms) of
                        (V.KChar 'e', []) -> do
                            let selectedTaskContent = selectedTask ^. taskContent
                            taskEditor .= editor (TaskEdit Edit) (Just 5) selectedTaskContent
                            changeFocus (TaskEdit Edit) s
                        (V.KChar 'c', [V.MCtrl]) -> do
                            modifiedTaskList <- liftIO $ updateTaskList (ChangeTaskCompletion selectedTask)
                            taskList .= BL.listReplace (DV.fromList modifiedTaskList) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                        (V.KDel, []) -> do
                            modifiedTaskList <- liftIO $ updateTaskList (DeleteTask selectedTask)
                            if selectedIndex - 1 == length modifiedTaskList
                                then taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just selectedIndex) (s ^. taskList)
                                else
                                    if selectedIndex == 0
                                        then taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just 0) (s ^. taskList)
                                        else taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just $ length modifiedTaskList - 1) (s ^. taskList)
                        (V.KChar 't', []) -> do
                            changeFocus (TaskEdit Insert) s
                        (V.KChar 'q', []) -> do
                            halt
                        (V.KChar 's', []) -> do
                            startStopSoundActive <- liftIO readStartStopSound
                            when startStopSoundActive $
                                if s ^. timerRunning
                                    then do
                                        _ <- liftIO $ forkIO $ NT.playAudio Stop
                                        return ()
                                    else do
                                        _ <- liftIO $ forkIO $ NT.playAudio Start
                                        return ()
                            timerRunning .= not (s ^. timerRunning)
                        (V.KChar 'r', []) -> do
                            initialTimer <- liftIO $ readInitialTimer timer
                            case timer of
                                Pomodoro -> do
                                    pomodoroTimer .= initialTimer
                                ShortBreak -> do
                                    shortBreakTimer .= initialTimer
                                LongBreak -> do
                                    longBreakTimer .= initialTimer
                        (V.KBackTab, []) -> do
                            timerRunning .= False
                            case timer of
                                Pomodoro -> changeFocus (TaskList ShortBreak) s
                                ShortBreak -> changeFocus (TaskList LongBreak) s
                                LongBreak -> changeFocus (TaskList Pomodoro) s
                        (V.KChar 'p', []) -> changeFocus Config s
                        _ -> BT.zoom taskList $ BL.handleListEventVi BL.handleListEvent vev
                Just Config -> do
                    case (k, ms) of
                        (V.KChar 'q', []) -> changeFocus (TaskList Pomodoro) s
                        (V.KEsc, []) -> changeFocus (TaskList Pomodoro) s
                        (V.KEnter, []) -> do
                            let selectedConfigElement = BL.listSelectedElement (s ^. configList)
                                (_, selectedConfigSetting) = case selectedConfigElement of
                                    Just (idx, configSetting) -> (idx, configSetting)
                                    _ -> (-1, ConfigSetting{_configLabel = "", _configValue = ConfigInitialTimer Pomodoro 0})
                            case selectedConfigSetting ^. configValue of
                                ConfigTimerStartStopSound _ -> do
                                    updatedConfigSettings <- liftIO $ updateConfig ToggleStartStopSound
                                    configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList)
                                ConfigTimerNotificationAlert _ -> do
                                    updatedConfigSettings <- liftIO $ updateConfig ToggleTimerNotificationAlert
                                    configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList)
                                ConfigTimerSoundAlert _ -> do
                                    updatedConfigSettings <- liftIO $ updateConfig ToggleTimerSoundAlert
                                    configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList)
                                ConfigInitialTimer timer _ -> do
                                    initialTimerConfigDialog .= initialTimerDialog (Just 0) timer
                                    changeFocus (InitialTimerDialog timer) s
                                ConfigTasksFilePath _ -> do
                                    changeFocus TasksFilePathBrowser s
                        _ -> BT.zoom configList $ BL.handleListEventVi BL.handleListEvent vev
                Just (InitialTimerDialog timer) -> do
                    case (k, ms) of
                        (V.KUp, []) -> do
                            updatedConfigSettings <- liftIO $ updateConfig (AddInitialTimer timer 60)
                            configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList)
                        (V.KDown, []) -> do
                            updatedConfigSettings <- liftIO $ updateConfig (AddInitialTimer timer (-60))
                            configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList)
                        (V.KEnter, []) -> case dialogSelection (s ^. initialTimerConfigDialog) of
                            Just CloseInitialTimerDialog -> changeFocus Config s
                            _ -> return ()
                        _ -> BT.zoom initialTimerConfigDialog $ handleDialogEvent vev
                Just TasksFilePathBrowser -> do
                    if not $ fileBrowserIsSearching (s ^. tasksFilePathBrowser) then do
                        case (k, ms) of
                            (V.KEsc, []) -> changeFocus Config s
                            (V.KChar 'q', []) -> changeFocus Config s
                            (V.KEnter, []) -> do
                                case fileBrowserCursor (s ^. tasksFilePathBrowser) of
                                    Just fileInfo -> do
                                        if fileType fileInfo == Just RegularFile then do
                                            updatedConfigSettings <- liftIO $ updateConfig $ SetTasksFilePath (fileInfoFilePath fileInfo)
                                            configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList)
                                            changeFocus Config s
                                        else
                                            BT.zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                                    Nothing -> BT.zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                                return ()
                            _ -> BT.zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                    else BT.zoom tasksFilePathBrowser $ handleFileBrowserEvent vev
                _ -> return ()
        _ -> return ()

fileType :: FileInfo -> Maybe FileType
fileType fileInfo = case fileInfoFileStatus fileInfo of
    Left _ -> Nothing
    Right fileStatus -> fileStatusFileType fileStatus

tickTimer :: Lens' AppState Int -> Int -> EventM Name AppState ()
tickTimer timerL timerValue = timerL .= max (timerValue - 1) 0

stopTimer :: EventM Name AppState ()
stopTimer = timerRunning .= False

timerEndedAudioAlert :: EventM Name AppState ()
timerEndedAudioAlert  = do
    timerEndedAudioAlertIsActive <- liftIO readTimerSoundAlert
    when timerEndedAudioAlertIsActive $ do
        _ <- liftIO $ forkIO $ NT.playAudio TimerEnded
        return ()

timerEndedNotificationAlert :: String -> EventM Name AppState ()
timerEndedNotificationAlert msg = do
    timerEndedNotificationAlertIsActive <- liftIO readTimerNotificationAlert
    when timerEndedNotificationAlertIsActive $ do
        NT.alertRoundEnded msg
        return ()

resetTimer :: Lens' AppState Int -> Timer -> EventM Name AppState ()
resetTimer timer currentTimer = do
    initialTimer <- liftIO $ CFG.readInitialTimer currentTimer
    timer .= initialTimer

increasePomodoroCycleCounter :: AppState -> EventM Name AppState ()
increasePomodoroCycleCounter s = do
    pomodoroCyclesCounter .= (s ^. pomodoroCyclesCounter) + 1
    updatedCycleCounter <- uses pomodoroCyclesCounter id
    if updatedCycleCounter == 4
        then do
            changeFocus (TaskList LongBreak) s
        else changeFocus (TaskList ShortBreak) s

saveTask :: Txt.Text -> Task -> TaskAction -> AppState -> EventM Name AppState ()
saveTask taskEditorContent selectedTask action s = do
    taskAlreadyExists <- liftIO $ taskExists taskEditorContent
    if not (Txt.null taskEditorContent) && not taskAlreadyExists
        then do
            let taskOperation = case action of
                    Insert -> AppendTask $ mkTask taskEditorContent
                    Edit -> EditTask selectedTask taskEditorContent
            updatedTasks <- liftIO $ updateTaskList taskOperation
            taskList .= BL.listReplace (DV.fromList updatedTasks) (BL.listSelected $ s ^. taskList) (s ^. taskList)
            taskEditor .= editor (TaskEdit action) (Just 5) ""
            changeFocus (TaskList Pomodoro) s
        else do
            taskEditor .= editor (TaskEdit action) (Just 5) ""
            changeFocus (TaskList Pomodoro) s