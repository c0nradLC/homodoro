{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Resources
  ( Name (..),
    Timer (..),
    TaskAction (..),
    AppState (..),
    Tick (..),
    ConfigFile (..),
    ConfigSetting (..),
    ConfigSettingValue (..),
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
    taskEditor,
    taskList,
    configList,
    focus,
    taskContent,
    taskCompleted,
    configLabel,
    configValue,
    pomodoroInitialTimer,
    shortBreakInitialTimer,
    longBreakInitialTimer,
    tasksFilePath,
    startStopSound,
    extractFilePathValue,
    extractInitialTimerValue,
    extractStartStopSoundValue,
    configSettingsValueToText,
    getConfigFileSettings
  )
where

import qualified Brick.Focus as BF
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.List as BL
import Control.Lens
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Text as T
import qualified Control.Applicative as FP

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
  { _taskContent :: T.Text,
    _taskCompleted :: Bool
  }
deriveJSON defaultOptions ''Task
makeLenses ''Task

instance Eq Task where
  (==) :: Task -> Task -> Bool
  (Task taskContent1 _) == (Task taskContent2 _) =
    taskContent1 == taskContent2

data TaskListOperation
  = AppendTask Task
  | DeleteTask Task
  | EditTask Task T.Text
  | ChangeTaskCompletion Task

type TaskListUpdate = TaskListOperation -> IO [Task]

data ConfigSettingValue 
  = ConfigInitialTimer Int
  | ConfigStartStopSound Bool
  | ConfigTasksFilePath FilePath
  deriving (Show, Eq)
makeLenses ''ConfigSettingValue
deriveJSON defaultOptions ''ConfigSettingValue

configSettingsValueToText :: ConfigSettingValue -> T.Text
configSettingsValueToText (ConfigInitialTimer i) = T.pack $ show i
configSettingsValueToText (ConfigStartStopSound b) = T.pack $ show b
configSettingsValueToText (ConfigTasksFilePath p) = T.pack $ show p

data ConfigSetting = ConfigSetting
  { _configLabel :: T.Text
  , _configValue :: ConfigSettingValue
  } deriving (Show, Eq)
makeLenses ''ConfigSetting
deriveJSON defaultOptions ''ConfigSetting

data ConfigFile = ConfigFile
  { _pomodoroInitialTimer   :: ConfigSetting
  , _shortBreakInitialTimer :: ConfigSetting
  , _longBreakInitialTimer  :: ConfigSetting
  , _tasksFilePath          :: ConfigSetting
  , _startStopSound         :: ConfigSetting
  } deriving (Show, Eq)
makeLenses ''ConfigFile
deriveJSON defaultOptions ''ConfigFile

getConfigFileSettings :: ConfigFile -> [ConfigSetting]
getConfigFileSettings configFile =
  [ configFile ^. pomodoroInitialTimer
  , configFile ^. shortBreakInitialTimer
  , configFile ^. longBreakInitialTimer
  , configFile ^. tasksFilePath
  , configFile ^. startStopSound
  ]

extractInitialTimerValue :: ConfigSetting -> Int
extractInitialTimerValue (ConfigSetting _ (ConfigInitialTimer initialTimer)) = initialTimer
extractInitialTimerValue _ = 0

extractFilePathValue :: ConfigSetting -> FilePath
extractFilePathValue (ConfigSetting _ (ConfigTasksFilePath path)) = path
extractFilePathValue _ = FP.empty

extractStartStopSoundValue :: ConfigSetting -> Bool
extractStartStopSoundValue (ConfigSetting _ (ConfigStartStopSound value)) = value
extractStartStopSoundValue _ = False

data ConfigFileOperation
  = AddInitialTimer Timer Int
  | ToggleStartStopSound

type ConfigFileUpdate = ConfigFileOperation -> IO [ConfigSetting]

data AppState = AppState
  { _timerRunning :: Bool,
    _pomodoroCounter :: Int,
    _pomodoroCyclesCounter :: Int,
    _pomodoroTimer :: Int,
    _shortBreakTimer :: Int,
    _longBreakTimer :: Int,
    _taskEditor :: Editor T.Text Name,
    _taskList :: BL.List Name Task,
    _focus :: BF.FocusRing Name,
    _configList :: BL.List Name ConfigSetting
  }
makeLenses ''AppState
