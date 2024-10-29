{-# LANGUAGE OverloadedStrings #-}

module UI.EventHandler (handleEvent) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt)
import qualified Brick as BT
import qualified Brick.Focus as BF
import Brick.Widgets.Edit (editor, getEditContents)
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import Config (getInitialTimer, updateConfig, configFileSettings)
import Control.Lens ((.=), (^.))
import Control.Monad.State (MonadIO (liftIO), MonadState (..), when)
import qualified Data.Text as Txt
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import qualified Graphics.Vty as VE
import Resources (AppState, ConfigFileOperation (..), Name (Commands, TaskEdit, TaskList, Config, InitialTimerDialog), TaskAction (Edit, Insert), TaskListOperation (AppendTask, ChangeTaskCompletion, DeleteTask, EditTask), Tick (Tick), Timer (LongBreak, Pomodoro, ShortBreak), focus, longBreakTimer, pomodoroTimer, shortBreakTimer, taskContent, taskEditor, taskList, timerRunning, configList, ConfigSetting (ConfigSetting, _configLabel, _configValue), ConfigSettingValue (..), configValue, initialTimerDialog, TimerDialogChoice (..))
import Task (mkTask, taskExists, updateTaskList)
import Timer (tickTimer)
import Brick.Widgets.Dialog (handleDialogEvent, dialogSelection)
import UI.Config (timerDialog)

handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent ev = do
  s <- get
  let currentFocus = BF.focusGetCurrent $ s ^. focus
  case ev of
    (AppEvent Tick) -> do
      when (s ^. timerRunning) $ do
        tickTimer currentFocus s
    (VtyEvent vev@(VE.EvKey k ms)) -> do
      let selectedListTask  = BL.listSelectedElement (s ^. taskList)
          taskEditorContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
          (selectedIndex, selectedTask)      = case selectedListTask of
            Just (idx, task) -> (idx, task)
            _ -> (-1, mkTask "")
      case currentFocus of
        Just (TaskEdit action) -> do
          case (k, ms) of
            (V.KIns, []) -> do
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
                (V.KChar 'c', []) -> do
                  changeFocus Commands s
                (V.KChar 's', []) -> do
                  timerRunning .= not (s ^. timerRunning)
                (V.KChar 'r', []) -> do
                  initialTimer <- liftIO $ getInitialTimer timer
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
        Just Commands ->
          case (k, ms) of
            (V.KChar 'p', []) -> changeFocus Config s
            _ -> changeFocus (TaskList Pomodoro) s
        Just Config -> do
          case (k, ms) of
            (V.KChar 'q', []) ->
              changeFocus (TaskList Pomodoro) s
            (V.KEsc, []) -> changeFocus (TaskList Pomodoro) s
            (V.KEnter, []) -> do
              let selectedConfigElement         = BL.listSelectedElement (s ^. configList)
                  (_, selectedConfigSetting)    = case selectedConfigElement of
                    Just (idx, configSetting) -> (idx, configSetting)
                    _ -> (-1, ConfigSetting {_configLabel = "", _configValue = ConfigInitialTimer Pomodoro 0})
              case selectedConfigSetting ^. configValue of
                ConfigStartStopSound _ -> do
                  updatedConfigSettings <- liftIO $ updateConfig ToggleStartStopSound
                  configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList )
                ConfigInitialTimer timer _ ->
                  changeFocus (InitialTimerDialog timer) s
                _ -> return()
            _ -> BT.zoom configList $ BL.handleListEventVi BL.handleListEvent vev
        Just (InitialTimerDialog timer) -> do
          initialTimerDialog .= timerDialog timer
          case (k, ms) of
            (V.KUp, []) -> do
              updatedConfigSettings <- liftIO $ updateConfig (AddInitialTimer timer 60)
              configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList )
            (V.KDown, []) -> do
              updatedConfigSettings <- liftIO $ updateConfig (AddInitialTimer timer (-60))
              configList .= BL.listReplace (DV.fromList $ configFileSettings updatedConfigSettings) (BL.listSelected $ s ^. configList) (s ^. configList )
            (V.KEnter, []) -> case dialogSelection (s ^. initialTimerDialog) of
              Just Ok -> changeFocus Config s
              _ -> return ()
            _ -> BT.zoom initialTimerDialog $ handleDialogEvent vev
        _ -> return ()
    _ -> return ()

changeFocus :: Name -> AppState -> EventM Name AppState ()
changeFocus nextFocus s = focus .= BF.focusSetCurrent nextFocus (s ^. focus)