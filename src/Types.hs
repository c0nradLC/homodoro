{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types (
    Name (..),
    Timer (..),
    TaskAction (..),
    AppState (..),
    Tick (..),
    ConfigFile (..),
    ConfigSetting (..),
    ConfigSettingValue (..),
    ConfigFileOperation (..),
    Task (..),
    TaskListOperation (..),
    InitialTimerDialogChoice (..),
    Audio (..),
    AudioProvider (..),
    NotificationProvider (..),
    SoundVolumeDialogChoice (..),
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
    timerPopupAlert,
    initialTimerConfigDialog,
    tasksFilePathBrowser,
    timerAlertSoundVolume,
    currentAlertSoundVolume,
    alertSoundVolumeConfigDialog,
    currentTimerTickSoundVolume,
    timerTickSoundVolume,
    timerTickSoundVolumeConfigDialog,
    notificationProvider,
    audioProvider,
    audioDirectoryPath,
    audioDirectoryPathSetting,
    audioDirectoryPathBrowser,
)
where

import qualified Brick.Focus as BF
import Brick.Widgets.Dialog (Dialog)
import Brick.Widgets.Edit (Editor)
import Brick.Widgets.FileBrowser (FileBrowser)
import qualified Brick.Widgets.List as BL
import Control.Lens (Lens', makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Text (Text)

data Timer
    = Pomodoro
    | ShortBreak
    | LongBreak
    deriving (Show, Eq, Ord)

deriveJSON defaultOptions ''Timer

data InitialTimerDialogChoice
    = CloseInitialTimerDialog

data SoundVolumeDialogChoice
    = PlayTestAudio
    | CloseSoundVolumeDialog

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
    | AudioDirectoryPathBrowser
    deriving (Show, Eq, Ord)

data Tick = Tick

data Audio
    = TimerEnded
    | TimerTick
    | TimerStart
    | TimerStop
    | None
    deriving (Show, Eq)

data AudioProvider
    = MPV
    | FFPlay
instance (Show AudioProvider) where
    show :: AudioProvider -> String
    show MPV = "mpv"
    show FFPlay = "ffplay"

data NotificationProvider
    = NotifySend
    | Zenity
instance (Show NotificationProvider) where
    show :: NotificationProvider -> String
    show NotifySend = "notify-send"
    show Zenity = "zenity"

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
    | ConfigTimerStartStopSound Bool
    | ConfigTasksFilePath FilePath
    | ConfigTimerPopupAlert Bool
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
    { _pomodoroInitialTimer :: ConfigSetting
    , _shortBreakInitialTimer :: ConfigSetting
    , _longBreakInitialTimer :: ConfigSetting
    , _startStopSound :: ConfigSetting
    , _tasksFilePath :: ConfigSetting
    , _timerPopupAlert :: ConfigSetting
    , _timerAlertSoundVolume :: ConfigSetting
    , _timerTickSoundVolume :: ConfigSetting
    , _audioDirectoryPathSetting :: ConfigSetting
    }
    deriving (Show, Eq)
makeLenses ''ConfigFile
deriveJSON defaultOptions ''ConfigFile

data ConfigFileOperation
    = AddInitialTimer Timer Int
    | ToggleStartStopSound
    | ToggleTimerPopupAlert
    | AddSoundVolume (Lens' ConfigFile ConfigSetting) Int
    | SetFilePathSetting (Lens' ConfigFile ConfigSetting) ConfigSettingValue

deriveJSON defaultOptions ''Audio

data AppState = AppState
    { _timerRunning :: Bool
    , _pomodoroCounter :: Int
    , _pomodoroCyclesCounter :: Int
    , _pomodoroTimer :: Int
    , _shortBreakTimer :: Int
    , _longBreakTimer :: Int
    , _taskEditor :: Editor Text Name
    , _taskList :: BL.List Name Task
    , _focus :: BF.FocusRing Name
    , _configList :: BL.List Name ConfigSetting
    , _initialTimerConfigDialog :: Dialog InitialTimerDialogChoice
    , _tasksFilePathBrowser :: FileBrowser Name
    , _currentAlertSoundVolume :: Int
    , _alertSoundVolumeConfigDialog :: Dialog SoundVolumeDialogChoice
    , _currentTimerTickSoundVolume :: Int
    , _timerTickSoundVolumeConfigDialog :: Dialog SoundVolumeDialogChoice
    , _notificationProvider :: Maybe NotificationProvider
    , _audioProvider :: Maybe AudioProvider
    , _audioDirectoryPath :: FilePath
    , _audioDirectoryPathBrowser :: FileBrowser Name
    }
makeLenses ''AppState
