{-# LANGUAGE OverloadedStrings #-}

module UI.UI (uiMain) where

import Brick
  ( App (..),
    BrickEvent (AppEvent, VtyEvent),
    EventM,
    Padding (Pad),
    Widget,
    customMain,
    hBox,
    halt,
    padLeft,
    padTop,
    showFirstCursor,
    str,
    txt,
    txtWrap,
    vBox,
    withAttr,
    withBorderStyle,
    (<=>), padTopBottom, strWrap, emptyWidget,
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Focus as BF
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Edit
  ( editor,
    getEditContents,
    renderEditor,
  )
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((+=), (.=), (^.))
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get),
    forever,
    void, when,
  )
import Data.Maybe (fromMaybe)
import qualified Data.Text as Txt
import qualified Data.Vector as DV
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as VE
import qualified Notify as NT
import qualified Task.File as TKF
import qualified Task.Task as TK
import Text.Printf (printf)
import Resources
  ( AppState (..),
    Name (..),
    Tick (..),
    currentTimer,
    focus,
    initialTimer,
    timerRunning,
    taskEditor,
    taskList,
  )
import UI.Attributes
  ( attributes,
    selectedTaskAttr,
    timerAttr,
  )
import Task.File (TaskListOperation(..))

uiMain :: IO ()
uiMain = do
  eventChan <- newBChan 10
  tasksFilePath <- TKF.getTasksFilePath
  TKF.createTasksFileIfNotExists tasksFilePath
  _ <- forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 1000000
  initialState <- createAppState 1500
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just eventChan) app initialState

createAppState :: Int -> IO AppState
createAppState timerDuration = do
  tasks <- TKF.getTasks
  let appState = createAppStateWithTasks timerDuration tasks
  return appState

createAppStateWithTasks :: Int -> Maybe [TK.Task] -> AppState
createAppStateWithTasks timerDuration maybeTasks =
  let tasksList = fromMaybe [] maybeTasks
      taskListWidget = BL.list TaskList (DV.fromList tasksList) 1
   in AppState
        { _timerRunning = False,
          _initialTimer = timerDuration,
          _currentTimer = timerDuration,
          _taskEditor = editor TaskEdit (Just 5) "",
          _focus = BF.focusRing [TaskList, TaskInsert, TaskEdit, Commands],
          _taskList = taskListWidget
        }

app :: App AppState Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const attributes
    }

handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent ev =
  case ev of
    (AppEvent Tick) -> do
      s <- get
      tickTimer s
      checkTimerEnded s
    (VtyEvent vev@(VE.EvKey k ms)) -> do
      s <- get
      case BF.focusGetCurrent $ s ^. focus of
        Just TaskInsert ->
          case (k, ms) of
            (V.KIns, []) -> do
              let insertedContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
              taskAlreadyExists <- liftIO $ TKF.taskExists insertedContent
              if not taskAlreadyExists && not (Txt.null insertedContent)
                then do
                  let task = TK.mkTask insertedContent $ Just False
                  taskList .= BL.listInsert (length $ s ^. taskList) task (s ^. taskList)
                  _ <- liftIO $ TKF.writeTasks TKF.updateTaskList (AppendTask task)
                  taskEditor .= editor TaskInsert (Just 5) ""
                  focus .= BF.focusSetCurrent TaskList (s ^. focus)
                else do
                  taskEditor .= editor TaskInsert (Just 5) ""
                  focus .= BF.focusSetCurrent TaskList (s ^. focus)
            (V.KEsc, []) -> do
              focus .= BF.focusSetCurrent TaskList (s ^. focus)
            _ -> do
              BT.zoom taskEditor $ BE.handleEditorEvent ev
        Just TaskEdit ->
          case (k, ms) of
            (V.KIns, []) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (_, selectedTask) -> do
                  let editedContent = Txt.strip $ Txt.unlines $ getEditContents (s ^. taskEditor)
                  taskAlreadyExists <- liftIO $ TKF.taskExists editedContent
                  when (not taskAlreadyExists && not (Txt.null editedContent)) $ do
                    modifiedTaskList <- liftIO $ TKF.writeTasks TKF.updateTaskList (EditTask selectedTask editedContent)
                    taskList .= BL.listReplace (DV.fromList modifiedTaskList) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                    taskEditor .= editor TaskEdit (Just 5) ""
                    focus .= BF.focusSetCurrent TaskList (s ^. focus)
                Nothing -> return ()
            (V.KEsc, []) -> do
              taskEditor .= editor TaskInsert (Just 5) ""
              focus .= BF.focusSetCurrent TaskList (s ^. focus)
            _ -> do
              BT.zoom taskEditor $ BE.handleEditorEvent ev
        Just TaskList ->
          case (k, ms) of
            (V.KChar 'q', []) -> do
              halt
            (V.KChar 'c', []) -> do
              focus .= BF.focusSetCurrent Commands (s ^. focus)
            (V.KChar 's', []) -> do
              timerRunning .= not (s ^. timerRunning)
            (V.KChar 'r', []) -> do
              currentTimer .= s ^. initialTimer
            (V.KChar 'i', []) -> do
              initialTimer += 60
              currentTimer += 60
            (V.KChar 'd', []) -> do
              initialTimer .= max ((s ^. initialTimer) - 60) 1
              currentTimer .= max ((s ^. currentTimer) - 60) 1
            (V.KChar 'I', []) -> do
              initialTimer += 10
              currentTimer += 10
            (V.KChar 'D', []) -> do
              initialTimer .= max ((s ^. initialTimer) - 10) 1
              currentTimer .= max ((s ^. currentTimer) - 10) 1
            (V.KChar 'e', []) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (_, selectedTask) -> do
                  let selectedTaskContent = selectedTask ^. TK.taskContent
                  taskEditor .= editor TaskEdit (Just 5) selectedTaskContent
                  focus .= BF.focusSetCurrent TaskEdit (s ^. focus)
                Nothing -> return ()
            (V.KChar 'c', [V.MCtrl]) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (_, selectedTask) -> do
                  modifiedTaskList <- liftIO $ TKF.writeTasks TKF.updateTaskList (ChangeTaskCompletion selectedTask)
                  taskList .= BL.listReplace (DV.fromList modifiedTaskList) (BL.listSelected $ s ^. taskList) (s ^. taskList)
                Nothing -> return ()
            (V.KDel, []) -> do
              let selectedListTask = BL.listSelectedElement (s ^. taskList)
              case selectedListTask of
                Just (selectedIndex, selectedTask) -> do
                  modifiedTaskList <- liftIO $ TKF.writeTasks TKF.updateTaskList (DeleteTask selectedTask)
                  if selectedIndex - 1 == length modifiedTaskList
                    then taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just selectedIndex) (s ^. taskList)
                  else if selectedIndex == 0
                    then taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just 0) (s ^. taskList)
                  else taskList .= BL.listReplace (DV.fromList modifiedTaskList) (Just $ length modifiedTaskList - 1) (s ^. taskList)
                Nothing -> return ()
            (V.KIns, []) -> do
              focus .= BF.focusSetCurrent TaskInsert (s ^. focus)
            _ -> BT.zoom taskList $ BL.handleListEventVi BL.handleListEvent vev
        Just Commands ->
          focus .= BF.focusSetCurrent TaskList (s ^. focus)
        _ -> return ()
    _ -> return ()

tickTimer :: AppState -> EventM Name AppState ()
tickTimer s
  | s ^. timerRunning = currentTimer .= max ((s ^. currentTimer) - 1) 0
  | otherwise = return ()

checkTimerEnded :: AppState -> EventM Name AppState ()
checkTimerEnded s
  | s ^. currentTimer == 0 = do
      currentTimer .= s ^. initialTimer
      timerRunning .= False
      _ <- liftIO $ forkIO NT.playAlertSound
      NT.alertRoundEnded
  | otherwise = return ()

drawUI :: AppState -> [Widget Name]
drawUI s =
  case BF.focusGetCurrent (s ^. focus) of
    currentFocus@(Just TaskInsert) -> [B.border (C.hCenter $ hBox [drawTimer s <=> drawTaskList s] <=> drawTaskEditor s) <=> drawCommands currentFocus]
    currentFocus@(Just TaskEdit) -> [B.border (C.hCenter $ hBox [drawTimer s <=> drawTaskList s] <=> drawTaskEditor s) <=> drawCommands currentFocus]
    Just Commands -> [B.border $ C.center drawCommandsScreen]
    _ -> [B.border $ C.hCenter $ drawTimer s <=> drawTaskList s <=> drawCommandsTip]

drawTimer :: AppState -> Widget Name
drawTimer s =
  padLeft (Pad 1) $
  padTopBottom 5 $
    C.hCenter $
      withAttr timerAttr $
        str $
          formatTimer $
            s ^. currentTimer

drawCommandsTip :: Widget Name
drawCommandsTip = C.hCenter $ str "press c to see the commands"

drawCommandsScreen :: Widget Name
drawCommandsScreen =
  C.hCenter $
      str
        " q        -> Quit application \n \
        \s        -> Start/Stop timer\n \
        \r        -> Reset timer\n \
        \i/d      -> Increase/Decrease timer by 1min\n \
        \I/D      -> Increased/Decrease timer by 10sec\n \
        \Insert   -> Add a task\n \
        \e        -> Edit a task\n \
        \Del      -> Delete a task\n \
        \Ctrl + C -> Change a task's status\n"

drawCommands :: Maybe Name -> Widget Name
drawCommands currentFocus = do
  case currentFocus of
    Just TaskInsert -> strWrap "ESC: Cancel task creation  Ins: Create task  "
    Just TaskEdit -> strWrap "ESC: Cancel task edition  Ins: Save task  "
    _ -> emptyWidget

drawTaskEditor :: AppState -> Widget Name
drawTaskEditor s =
  padTop (Pad 1) $
      C.hCenter $
        B.borderWithLabel (str "Task") $
          renderEditor drawTaskInsertorContent (BF.focusGetCurrent (s ^. focus) == Just TaskInsert) (s ^. taskEditor)

drawTaskInsertorContent :: [Txt.Text] -> Widget Name
drawTaskInsertorContent t = txt (Txt.unlines t)

drawTaskList :: AppState -> Widget Name
drawTaskList s =
  do
    B.borderWithLabel (str "Tasks")
    $ BL.renderList drawTaskListItem (BF.focusGetCurrent (s ^. focus) == Just TaskList) (s ^. taskList)

drawTaskListItem :: Bool -> TK.Task -> Widget Name
drawTaskListItem sel task
  | sel =
      withAttr selectedTaskAttr $
        padLeft (Pad 1) $
          taskListItem task
  | otherwise =
      taskListItem task

taskListItem :: TK.Task -> Widget Name
taskListItem task =
  vBox
    [ withBorderStyle BS.unicodeRounded $
        B.border $
          padLeft (Pad 1) $
            txtWrap (task ^. TK.taskContent)
              <=> if task ^. TK.taskCompleted then padTop (Pad 1) $ txtWrap "☒" else padTop (Pad 1) $ txtWrap "☐"
    ]

formatTimer :: Int -> String
formatTimer timer =
  let minutes = timer `div` 60
      seconds = timer `mod` 60
   in printf "%02d:%02d" minutes seconds
