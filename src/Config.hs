{-# LANGUAGE OverloadedStrings #-}

module Config (
    createConfigFileIfNotExists,
    readConfig,
    readTasksFilePath,
    readInitialTimer,
    readStartStopSound,
    readTimerNotificationAlert,
    readTimerSoundAlert,
    readSoundVolume,
    updateConfig,
    configFileSettings,
    configSettingsValueToText,
    defaultTasksFilePathIO,
    maybeConfigIntValue,
    maybeConfigBoolValue,
    configBoolValue,
    findConfigSetting,
    showBool,
    soundVolumePercentage
)
where

import qualified Control.Applicative as FP
import Control.Lens ((%~), (&), (.~))
import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as BSL
import Data.List (find)
import qualified Data.Text as T
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigSetting (..), ConfigSettingValue (..), Timer (..), configValue, longBreakInitialTimer, pomodoroInitialTimer, shortBreakInitialTimer, startStopSound, tasksFilePath, timerNotificationAlert, timerSoundAlert, soundVolume)
import qualified System.Directory as D
import qualified System.FilePath as FP
import UI.Timer (formatTimer)

defaultConfig :: IO ConfigFile
defaultConfig = do
    xdgDataPath <- D.getXdgDirectory D.XdgData ""
    return
        ConfigFile
            { _pomodoroInitialTimer = ConfigSetting{_configLabel = "Pomodoro round initial timer", _configValue = ConfigInitialTimer Pomodoro 1500}
            , _shortBreakInitialTimer = ConfigSetting{_configLabel = "Short break initial timer", _configValue = ConfigInitialTimer ShortBreak 300}
            , _longBreakInitialTimer = ConfigSetting{_configLabel = "Long break initial timer", _configValue = ConfigInitialTimer LongBreak 900}
            , _startStopSound = ConfigSetting{_configLabel = "Timer start/stop sound", _configValue = ConfigTimerStartStopSound False}
            , _tasksFilePath = ConfigSetting{_configLabel = "Tasks file path", _configValue = ConfigTasksFilePath $ xdgDataPath FP.</> "homodoro" FP.</> "tasks"}
            , _timerNotificationAlert = ConfigSetting{_configLabel = "Timer notification alert", _configValue = ConfigTimerNotificationAlert True}
            , _timerSoundAlert = ConfigSetting{_configLabel = "Timer sound alert", _configValue = ConfigTimerSoundAlert True}
            , _soundVolume = ConfigSetting{_configLabel = "Sound volume", _configValue = ConfigSoundVolume 60}
            }

createConfigFileIfNotExists :: IO ()
createConfigFileIfNotExists = do
    configFilePath <- xdgConfigFilePath
    D.createDirectoryIfMissing True (FP.takeDirectory configFilePath)
    fileExists <- D.doesFileExist configFilePath
    defaultConfigFile <- defaultConfig
    unless fileExists $ do BSL.writeFile configFilePath $ encode defaultConfigFile

xdgConfigFilePath :: IO FilePath
xdgConfigFilePath = do
    xdgConfigPath <- D.getXdgDirectory D.XdgConfig ""
    pure $ xdgConfigPath FP.</> "homodoro" FP.</> "config"

updateConfig :: ConfigFileOperation -> IO ConfigFile
updateConfig (AddInitialTimer timer time) = do
    configFile <- readConfig
    let updatedConfigFile = configFile & initialTimerL %~ addInitialTimer
    writeConfig updatedConfigFile
    return updatedConfigFile
  where
    addInitialTimer (ConfigInitialTimer _ val) = ConfigInitialTimer timer (max (val + time) 0)
    addInitialTimer val = val
    initialTimerL = case timer of
        Pomodoro -> pomodoroInitialTimer . configValue
        ShortBreak -> shortBreakInitialTimer . configValue
        LongBreak -> longBreakInitialTimer . configValue
updateConfig ToggleStartStopSound = do
    configFile <- readConfig
    let updatedConfigFile = configFile & startStopSound . configValue %~ toggleBool
    writeConfig updatedConfigFile
    return updatedConfigFile
updateConfig (SetTasksFilePath fp) = do
    configFile <- readConfig
    let updatedConfigFile = configFile & tasksFilePath . configValue .~ ConfigTasksFilePath fp
    writeConfig updatedConfigFile
    return updatedConfigFile
updateConfig ToggleTimerNotificationAlert = do
    configFile <- readConfig
    let updatedConfigFile = configFile & timerNotificationAlert . configValue %~ toggleBool
    writeConfig updatedConfigFile
    return updatedConfigFile
updateConfig ToggleTimerSoundAlert = do
    configFile <- readConfig
    let updatedConfigFile = configFile & timerSoundAlert . configValue %~ toggleBool
    writeConfig updatedConfigFile
    return updatedConfigFile
updateConfig (AddSoundVolume volStep) = do
    configFile <- readConfig
    let updatedConfigFile = configFile & soundVolume . configValue %~ addSoundVolumeStep
    writeConfig updatedConfigFile
    return updatedConfigFile
    where addSoundVolumeStep (ConfigSoundVolume val) = ConfigSoundVolume (min (max (val + volStep) 0) 128)
          addSoundVolumeStep val = val


toggleBool :: ConfigSettingValue -> ConfigSettingValue
toggleBool (ConfigTimerStartStopSound b) = ConfigTimerStartStopSound (not b)
toggleBool (ConfigTimerNotificationAlert b) = ConfigTimerNotificationAlert  (not b)
toggleBool (ConfigTimerSoundAlert b) = ConfigTimerSoundAlert (not b)
toggleBool val = val

writeConfig :: ConfigFile -> IO ()
writeConfig cfg = do
    configFilePath <- xdgConfigFilePath
    BSL.writeFile configFilePath $ encode cfg

readConfig :: IO ConfigFile
readConfig = do
    configFilePath <- xdgConfigFilePath
    configFileContent <- BSL.readFile configFilePath
    maybe defaultConfig return (decode configFileContent)

readTasksFilePath :: IO FilePath
readTasksFilePath = do
    configFile <- readConfig
    return $ configFilePathValue $ configFile ^. tasksFilePath

defaultTasksFilePathIO :: IO FilePath
defaultTasksFilePathIO = do
    xdgDirectory <- D.getXdgDirectory D.XdgData ""
    return $ xdgDirectory FP.</> "homodoro" FP.</> "tasks"

readInitialTimer :: Timer -> IO Int
readInitialTimer timer = do
    initialTimerValue <$> readConfig
    where initialTimerValue configFile = case timer of
            Pomodoro -> configIntValue (configFile ^. pomodoroInitialTimer)
            ShortBreak -> configIntValue (configFile ^. shortBreakInitialTimer)
            LongBreak -> configIntValue (configFile ^. longBreakInitialTimer)


readStartStopSound :: IO Bool
readStartStopSound = do
    configFile <- readConfig
    return $ configBoolValue $ configFile ^. startStopSound

readTimerNotificationAlert :: IO Bool
readTimerNotificationAlert = do
    configFile <- readConfig
    return $ configBoolValue $ configFile ^. timerNotificationAlert

readTimerSoundAlert :: IO Bool
readTimerSoundAlert = do
    configFile <- readConfig
    return $ configBoolValue $ configFile ^. timerSoundAlert

readSoundVolume :: IO Int
readSoundVolume = do
    configFile <- readConfig
    return $ maybeConfigIntValue $ Just (configFile ^. soundVolume)

configFileSettings :: ConfigFile -> [ConfigSetting]
configFileSettings configFile =
    [ configFile ^. pomodoroInitialTimer
    , configFile ^. shortBreakInitialTimer
    , configFile ^. longBreakInitialTimer
    , configFile ^. tasksFilePath
    , configFile ^. startStopSound
    , configFile ^. timerNotificationAlert
    , configFile ^. timerSoundAlert
    , configFile ^. soundVolume
    ]

findConfigSetting :: ConfigSettingValue -> [ConfigSetting] -> Maybe ConfigSetting
findConfigSetting configSettingValue =
    find (\setting -> case (configSettingValue, setting ^. configValue) of
        (ConfigSoundVolume _, ConfigSoundVolume _) -> True
        (ConfigInitialTimer timer _, ConfigInitialTimer settingTimer _) -> timer == settingTimer
        (ConfigTimerNotificationAlert _, ConfigTimerNotificationAlert _) -> True
        (ConfigTimerSoundAlert _, ConfigTimerSoundAlert _) -> True
        _ -> setting ^. configValue == configSettingValue)

configSettingsValueToText :: ConfigSettingValue -> T.Text
configSettingsValueToText (ConfigInitialTimer _ i) = T.pack $ formatTimer i
configSettingsValueToText (ConfigTimerStartStopSound b) = T.pack $ showBool b
configSettingsValueToText (ConfigTasksFilePath p) = T.pack $ show p
configSettingsValueToText (ConfigTimerNotificationAlert b) = T.pack $ showBool b
configSettingsValueToText (ConfigTimerSoundAlert b) = T.pack $ showBool b
configSettingsValueToText (ConfigSoundVolume vol) = T.pack $ soundVolumePercentage vol

showBool :: Bool -> String
showBool true = if true then "Enabled" else "Disabled"

maybeConfigIntValue :: Maybe ConfigSetting -> Int
maybeConfigIntValue (Just (ConfigSetting _ (ConfigInitialTimer _ initialTimer))) = initialTimer
maybeConfigIntValue (Just (ConfigSetting _ (ConfigSoundVolume currentSoundVolume))) = currentSoundVolume
maybeConfigIntValue _ = 0

configIntValue :: ConfigSetting -> Int
configIntValue (ConfigSetting _ (ConfigInitialTimer _ initialTimer)) = initialTimer
configIntValue (ConfigSetting _ (ConfigSoundVolume currentSoundVolume)) = currentSoundVolume
configIntValue _ = 0

configFilePathValue :: ConfigSetting -> FilePath
configFilePathValue (ConfigSetting _ (ConfigTasksFilePath path)) = path
configFilePathValue _ = FP.empty

maybeConfigBoolValue :: Maybe ConfigSetting -> Bool
maybeConfigBoolValue (Just (ConfigSetting _ (ConfigTimerNotificationAlert value))) = value
maybeConfigBoolValue (Just (ConfigSetting _ (ConfigTimerSoundAlert value))) = value
maybeConfigBoolValue (Just (ConfigSetting _ (ConfigTimerStartStopSound value))) = value
maybeConfigBoolValue _ = False

configBoolValue :: ConfigSetting -> Bool
configBoolValue (ConfigSetting _ (ConfigTimerNotificationAlert value)) = value
configBoolValue (ConfigSetting _ (ConfigTimerSoundAlert value)) = value
configBoolValue (ConfigSetting _ (ConfigTimerStartStopSound value)) = value
configBoolValue _ = False

soundVolumePercentage :: Int -> String
soundVolumePercentage vol = show (round((fromIntegral vol / 128 :: Double) * 100) :: Int) ++ "%"