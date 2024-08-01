{-# LANGUAGE OverloadedStrings #-}

module UI.EventHandler (handleEvent) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt)
import qualified Brick as BT
import qualified Brick.Focus as BF
import Brick.Widgets.Edit (editor, getEditContents)
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import Config (getConfig, getInitialTimer, updateConfig)
import Control.Concurrent (forkIO)
import Control.Lens ((+=), (.=), (^.))
import Control.Monad.State (MonadIO (liftIO), MonadState (..), when)
import qualified Data.Text as Txt
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import qualified Graphics.Vty as VE
import Resources (AppState, ConfigFileOperation (UpdateInitialTimer), Name (Commands, TaskEdit, TaskList), TaskAction (Edit, Insert), TaskListOperation (AppendTask, ChangeTaskCompletion, DeleteTask, EditTask), Tick (Tick), Timer (LongBreak, Pomodoro, ShortBreak), focus, longBreakTimer, pomodoroTimer, shortBreakTimer, taskContent, taskEditor, taskList, timerRunning)
import Task (getTasks, mkTask, taskExists, updateTaskList, writeTasks)
import Timer (tickTimer)

handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent ev = do
  s <- get
  case ev of
    (AppEvent Tick) -> do
      tickTimer s
    (VtyEvent vev@(VE.EvKey k ms)) -> do
      case BF.focusGetCurrent $ s ^. focus of
        Just (TaskEdit Insert) ->
          case (k, ms) of
            (V.KIns, []) -> do
              tasksFromFile <- liftIO getTasks
              let insertedContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
              if not (taskExists tasksFromFile insertedContent) && not (Txt.null insertedContent)
                then do
                  updatedTasks <- liftIO $ writeTasks updateTaskList (AppendTask (mkTask insertedContent))
                  taskList .= BL.listReplace (DV.fromList updatedTasks) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                  taskEditor .= editor (TaskEdit Insert) (Just 5) ""
                  changeFocus (TaskList Pomodoro) s
                else do
                  taskEditor .= editor (TaskEdit Insert) (Just 5) ""
                  changeFocus (TaskList Pomodoro) s
            (V.KEsc, []) -> do
              changeFocus (TaskList Pomodoro) s
            _ -> do
              BT.zoom taskEditor $ BE.handleEditorEvent ev
        Just (TaskEdit Edit) ->
          case (k, ms) of
            (V.KIns, []) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (_, selectedTask) -> do
                  tasksFromFile <- liftIO getTasks
                  let editedContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
                  when (not (taskExists tasksFromFile editedContent) && not (Txt.null editedContent)) $ do
                    updatedTasks <- liftIO $ writeTasks updateTaskList (EditTask selectedTask editedContent)
                    taskList .= BL.listReplace (DV.fromList updatedTasks) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                    taskEditor .= editor (TaskEdit Edit) (Just 5) ""
                    changeFocus (TaskList Pomodoro) s
                Nothing -> return ()
            (V.KEsc, []) -> do
              taskEditor .= editor (TaskEdit Insert) (Just 5) ""
              changeFocus (TaskList Pomodoro) s
            _ -> do
              BT.zoom taskEditor $ BE.handleEditorEvent ev
        Just cfs@(TaskList timer) -> do
          case (k, ms) of
            (V.KChar 'e', []) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (_, selectedTask) -> do
                  let selectedTaskContent = selectedTask ^. taskContent
                  taskEditor .= editor (TaskEdit Edit) (Just 5) selectedTaskContent
                  changeFocus (TaskEdit Edit) s
                Nothing -> return ()
            (V.KChar 'c', [V.MCtrl]) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (_, selectedTask) -> do
                  modifiedTaskList <- liftIO $ writeTasks updateTaskList (ChangeTaskCompletion selectedTask)
                  taskList .= BL.listReplace (DV.fromList modifiedTaskList) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                Nothing -> return ()
            (V.KDel, []) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (selectedIndex, selectedTask) -> do
                  modifiedTaskList <- liftIO $ writeTasks updateTaskList (DeleteTask selectedTask)
                  if selectedIndex - 1 == length modifiedTaskList
                    then taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just selectedIndex) (s ^. taskList)
                    else
                      if selectedIndex == 0
                        then taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just 0) (s ^. taskList)
                        else taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just $ length modifiedTaskList - 1) (s ^. taskList)
                Nothing -> return ()
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
            (V.KChar 'i', []) -> do
              config <- liftIO getConfig
              case timer of
                Pomodoro -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer Pomodoro 60) config
                  pomodoroTimer += 60
                ShortBreak -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer ShortBreak 60) config
                  shortBreakTimer += 60
                LongBreak -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer LongBreak 60) config
                  longBreakTimer += 60
            (V.KChar 'd', []) -> do
              config <- liftIO getConfig
              case cfs of
                TaskList Pomodoro -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer Pomodoro (- 60)) config
                  pomodoroTimer .= max ((s ^. pomodoroTimer) - 60) 0
                TaskList ShortBreak -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer ShortBreak (- 60)) config
                  shortBreakTimer .= max ((s ^. shortBreakTimer) - 60) 0
                TaskList LongBreak -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer LongBreak (- 60)) config
                  longBreakTimer .= max ((s ^. longBreakTimer) - 60) 0
            (V.KChar 'I', []) -> do
              config <- liftIO getConfig
              case cfs of
                TaskList Pomodoro -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer Pomodoro 10) config
                  pomodoroTimer += 10
                TaskList ShortBreak -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer ShortBreak 10) config
                  shortBreakTimer += 10
                TaskList LongBreak -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer LongBreak 10) config
                  longBreakTimer += 10
            (V.KChar 'D', []) -> do
              config <- liftIO getConfig
              case cfs of
                TaskList Pomodoro -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer Pomodoro (- 10)) config
                  pomodoroTimer .= max ((s ^. pomodoroTimer) - 10) 0
                TaskList ShortBreak -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer ShortBreak (- 10)) config
                  shortBreakTimer .= max ((s ^. shortBreakTimer) - 10) 0
                TaskList LongBreak -> do
                  _ <- liftIO $ forkIO $ do
                    updateConfig (UpdateInitialTimer LongBreak (- 10)) config
                  longBreakTimer .= max ((s ^. shortBreakTimer) - 10) 0
            (V.KBackTab, []) -> do
              timerRunning .= False
              case cfs of
                TaskList Pomodoro -> changeFocus (TaskList ShortBreak) s
                TaskList ShortBreak -> changeFocus (TaskList LongBreak) s
                TaskList LongBreak -> changeFocus (TaskList Pomodoro) s
            _ -> BT.zoom taskList $ BL.handleListEventVi BL.handleListEvent vev
        Just Commands ->
          changeFocus (TaskList Pomodoro) s
        _ -> return ()
    _ -> return ()

changeFocus :: Name -> AppState -> EventM Name AppState ()
changeFocus nextFocus s = focus .= BF.focusSetCurrent nextFocus (s ^. focus)
