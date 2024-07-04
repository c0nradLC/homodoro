{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

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
    pomodoroCycleCounter,
    pomodoroTimer,
    shortBreakTimer,
    longBreakTimer,
    shortBreakInitialTimer,
    longBreakInitialTimer,
    pomodoroInitialTimer,
    taskEditor,
    taskList,
    focus,
    activeTasksFilePath,
    taskContent,
    taskCompleted
)
where

import qualified Brick.Focus as BF
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.List as BL
import Control.Lens
import qualified Data.Text as Txt
import qualified Data.Text as T
import Data.Aeson.TH (deriveJSON, defaultOptions)

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
    { _taskContent :: T.Text
    , _taskCompleted :: Bool
    }
deriveJSON defaultOptions ''Task
makeLenses ''Task

data TaskListOperation
    = AppendTask Task
    | DeleteTask Task
    | EditTask Task T.Text
    | ChangeTaskCompletion Task

type TaskListUpdate = TaskListOperation -> [Task] -> [Task]

instance Eq Task where
    (==) :: Task -> Task -> Bool
    (Task content1 _) == (Task content2 _) =
        content1 == content2

data ConfigFile = ConfigFile
    { _pomodoroInitialTimer :: Int
    , _shortBreakInitialTimer :: Int
    , _longBreakInitialTimer :: Int
    , _activeTasksFilePath :: FilePath
    , _archivedTasksFilePath :: FilePath
    , _dailyTasksMode :: Bool --This will make tasks be grouped by date, just like the archived tasks
    }
deriveJSON defaultOptions ''ConfigFile
makeLenses ''ConfigFile

data ConfigFileOperation
    = ToggleDailyTasksMode
    | UpdateInitialTimer Timer Int
    | SetActiveTasksFilePath FilePath
    | SetArchivedTasksFilePath FilePath

type ConfigFileUpdate = ConfigFileOperation -> ConfigFile -> ConfigFile

data AppState = AppState
    { _timerRunning :: Bool
    , _pomodoroCycleCounter :: Int
    , _pomodoroTimer :: Int
    , _shortBreakTimer :: Int
    , _longBreakTimer :: Int
    , _taskEditor :: Editor Txt.Text Name
    , _taskList :: BL.List Name Task
    , _focus :: BF.FocusRing Name
    }
makeLenses ''AppState