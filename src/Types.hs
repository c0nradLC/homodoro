{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types (
    Name (..),
    Timer (..),
    Timers (..),
    TimerState (..),
    TaskAction (..),
    AppState (..),
    Tick (..),
    ConfigFile (..),
    ConfigSetting (..),
    ConfigSettingValue (..),
    Task (..),
    TaskListOperation (..),
    InitialTimerDialogChoice (..),
    Audio (..),
    SoundVolumeDialogChoice (..),
    timerRunning,
    pomodoroCounter,
    pomodoroCyclesCounter,
    taskEditor,
    taskList,
    configList,
    focus,
    taskContent,
    taskCompleted,
    configLabel,
    configValue,
    pomodoroInitialTimerSetting,
    shortBreakInitialTimerSetting,
    longBreakInitialTimerSetting,
    tasksFilePath,
    tasksFilePathSetting,
    timerStartStopSoundVolumeSetting,
    timerPopupAlertSetting,
    initialTimerConfigDialog,
    tasksFilePathBrowser,
    timerAlertSoundVolumeSetting,
    timerAlertSoundVolume,
    timerAlertSoundVolumeConfigDialog,
    timerTickSoundVolume,
    timerTickSoundVolumeSetting,
    timerTickSoundVolumeConfigDialog,
    audioDirectoryPath,
    audioDirectoryPathSetting,
    audioDirectoryPathBrowser,
    configFile,
    timerPopupAlert,
    timerStartStopSoundVolume,
    timerStartStopSoundVolumeConfigDialog,
    timers,
    pomodoroState,
    shortBreakState,
    longBreakState,
    timerCurrentValue,
    timerInitialValue
)
where

import qualified Brick.Focus as BF
import Brick.Widgets.Dialog (Dialog)
import Brick.Widgets.Edit (Editor)
import Brick.Widgets.FileBrowser (FileBrowser)
import qualified Brick.Widgets.List as BL
import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Text (Text)

data Timer
    = Pomodoro
    | ShortBreak
    | LongBreak
    deriving (Show, Eq, Ord)
deriveJSON defaultOptions ''Timer

data TimerState = TimerState
    { _timerCurrentValue :: Int
    , _timerInitialValue :: Int
    }
makeLenses ''TimerState

data Timers = Timers
    { _pomodoroState :: TimerState
    , _shortBreakState :: TimerState
    , _longBreakState :: TimerState
    }
makeLenses ''Timers

data InitialTimerDialogChoice
    = SaveInitialTimer
    | CloseInitialTimerDialog

data SoundVolumeDialogChoice
    = PlayTestAudio
    | CloseSoundVolumeDialog
    | SaveSoundVolume

data TaskAction
    = Edit
    | Insert
    deriving (Show, Eq, Ord)

data Name
    = TaskEdit TaskAction
    | TaskList Timer
    | Config
    | InitialTimerDialog Timer
    | TasksFilePathBrowser
    | TimerAlertSoundVolumeDialog
    | TimerTickSoundVolumeDialog
    | TimerStartStopSoundVolumeDialog
    | AudioDirectoryPathBrowser
    deriving (Show, Eq, Ord)

data Tick = Tick

data Audio
    = TimerAlert
    | TimerTick
    | TimerStartStop
    deriving (Show, Eq)

data Task = Task
    { _taskContent :: Text
    , _taskCompleted :: Bool
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
    | EditTask Task Text
    | ChangeTaskCompletion Task

data ConfigSettingValue
    = ConfigInitialTimer Timer Int
    | ConfigTasksFilePath FilePath
    | ConfigTimerPopupAlert Bool
    | ConfigTimerStartStopSoundVolume Int
    | ConfigTimerAlertSoundVolume Int
    | ConfigTimerTickSoundVolume Int
    | ConfigAudioDirectoryPath FilePath
    deriving (Show, Eq)

makeLenses ''ConfigSettingValue
deriveJSON defaultOptions ''ConfigSettingValue

data ConfigSetting = ConfigSetting
    { _configLabel :: String
    , _configValue :: ConfigSettingValue
    }
    deriving (Show, Eq)

makeLenses ''ConfigSetting
deriveJSON defaultOptions ''ConfigSetting

data ConfigFile = ConfigFile
    { _pomodoroInitialTimerSetting :: ConfigSetting
    , _shortBreakInitialTimerSetting :: ConfigSetting
    , _longBreakInitialTimerSetting :: ConfigSetting
    , _timerStartStopSoundVolumeSetting :: ConfigSetting
    , _tasksFilePathSetting :: ConfigSetting
    , _timerPopupAlertSetting :: ConfigSetting
    , _timerAlertSoundVolumeSetting :: ConfigSetting
    , _timerTickSoundVolumeSetting :: ConfigSetting
    , _audioDirectoryPathSetting :: ConfigSetting
    }
    deriving (Show, Eq)
makeLenses ''ConfigFile
deriveJSON defaultOptions ''ConfigFile

data AppState = AppState
    { _timerRunning :: Bool
    , _timers :: Timers
    , _pomodoroCounter :: Int
    , _pomodoroCyclesCounter :: Int
    , _taskEditor :: Editor Text Name
    , _taskList :: BL.List Name Task
    , _focus :: BF.FocusRing Name
    , _configList :: BL.List Name ConfigSetting
    , _configFile :: ConfigFile
    , _initialTimerConfigDialog :: Dialog InitialTimerDialogChoice
    , _tasksFilePath :: FilePath
    , _tasksFilePathBrowser :: FileBrowser Name
    , _timerPopupAlert :: Bool
    , _timerStartStopSoundVolume :: Int
    , _timerStartStopSoundVolumeConfigDialog :: Dialog SoundVolumeDialogChoice
    , _timerAlertSoundVolume :: Int
    , _timerAlertSoundVolumeConfigDialog :: Dialog SoundVolumeDialogChoice
    , _timerTickSoundVolume :: Int
    , _timerTickSoundVolumeConfigDialog :: Dialog SoundVolumeDialogChoice
    , _audioDirectoryPath :: FilePath
    , _audioDirectoryPathBrowser :: FileBrowser Name
    }
makeLenses ''AppState
