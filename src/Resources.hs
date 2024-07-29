{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Resources (
    Name (..),
    Timer (..),
    TaskAction (..),
    AppState (..),
    Tick (..),
    ConfigFile (..),
    ConfigFileUpdate,
    ConfigFileOperation (..),
    Task (..),
    TaskListUpdate,
    TaskListOperation (..),
    timerRunning,
    pomodoroCounter,
    pomodoroCyclesCounter,
    pomodoroTimer,
    shortBreakTimer,
    longBreakTimer,
    shortBreakInitialTimer,
    longBreakInitialTimer,
    pomodoroInitialTimer,
    taskEditor,
    taskList,
    focus,
    tasksFilePath,
    taskContent,
    taskCompleted
)
where

import qualified Brick.Focus as BF
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.List as BL
import Control.Lens
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Text as T
import qualified Data.Text as Txt
import Data.UUID (UUID)

data Timer
    = Pomodoro
    | ShortBreak
    | LongBreak
    deriving (Show, Eq, Ord)

data TaskAction
    = Edit
    | Insert
    deriving (Show, Eq, Ord)

data Name
    = TaskEdit TaskAction
    | TaskList Timer 
    | Commands
    | Config
    deriving (Show, Eq, Ord)

data Tick = Tick

data Task = Task
    { _taskId :: UUID
    , _taskContent :: T.Text
    , _taskCompleted :: Bool
    }
deriveJSON defaultOptions ''Task
makeLenses ''Task

instance Eq Task where
    (==) :: Task -> Task -> Bool
    (Task taskId1 _ _) == (Task taskId2 _ _) =
        taskId1 == taskId2

data TaskListOperation
    = AppendTask Task
    | DeleteTask Task
    | EditTask Task T.Text
    | ChangeTaskCompletion Task

type TaskListUpdate = TaskListOperation -> [Task] -> [Task]

data ConfigFile = ConfigFile
    { _pomodoroInitialTimer :: Int
    , _shortBreakInitialTimer :: Int
    , _longBreakInitialTimer :: Int
    , _muteAlarm :: Bool
    , _disableNotification :: Bool
    , _tasksFilePath :: FilePath
    }
deriveJSON defaultOptions ''ConfigFile
makeLenses ''ConfigFile

data ConfigFileOperation
    = UpdateInitialTimer Timer Int
    | SetTasksFilePath FilePath

type ConfigFileUpdate = ConfigFileOperation -> ConfigFile -> ConfigFile

data AppState = AppState
    { _timerRunning :: Bool
    , _pomodoroCounter :: Int
    , _pomodoroCyclesCounter :: Int
    , _pomodoroTimer :: Int
    , _shortBreakTimer :: Int
    , _longBreakTimer :: Int
    , _taskEditor :: Editor Txt.Text Name
    , _taskList :: BL.List Name Task
    , _focus :: BF.FocusRing Name
    }
makeLenses ''AppState
