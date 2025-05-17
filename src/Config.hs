{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Config (
    createProgramFileAndDirectoriesIfNotExists,
    readConfig,
    readTasksFilePath,
    readInitialTimer,
    readStartStopSound,
    readTimerPopupAlert,
    readAlertSoundVolume,
    readTimerTickSoundVolume,
    readAudioDirectoryPath,
    updateConfig,
    configFileSettings,
    configSettingsValueToString,
    defaultTasksFilePathIO,
    maybeConfigIntValue,
    maybeConfigBoolValue,
    configBoolValue,
    findConfigSetting,
    showBool,
    configFilePathValue,
)
where

import qualified Control.Applicative as FP
import Control.Lens (Lens', (%~), (&), (.~))
import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (find)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified System.Directory as D
import System.FilePath (takeDirectory)
import qualified System.FilePath as FP
import Types (ConfigFile (..), ConfigFileOperation (..), ConfigSetting (..), ConfigSettingValue (..), Timer (..), audioDirectoryPathSetting, configValue, longBreakInitialTimerSetting, pomodoroInitialTimerSetting, shortBreakInitialTimerSetting, startStopSoundSetting, tasksFilePathSetting, timerAlertSoundVolumeSetting, timerPopupAlertSetting, timerTickSoundVolumeSetting)
import UI.Timer (formatTimer)

defaultConfig :: IO ConfigFile
defaultConfig = do
    xdgDataPath <- D.getXdgDirectory D.XdgData ""
    return
        ConfigFile
            { _pomodoroInitialTimerSetting = ConfigSetting{_configLabel = "Pomodoro round initial timer", _configValue = ConfigInitialTimer Pomodoro 1500}
            , _shortBreakInitialTimerSetting = ConfigSetting{_configLabel = "Short break initial timer", _configValue = ConfigInitialTimer ShortBreak 300}
            , _longBreakInitialTimerSetting = ConfigSetting{_configLabel = "Long break initial timer", _configValue = ConfigInitialTimer LongBreak 900}
            , _startStopSoundSetting = ConfigSetting{_configLabel = "Timer start/stop sound", _configValue = ConfigTimerStartStopSound False}
            , _tasksFilePathSetting = ConfigSetting{_configLabel = "Tasks file path", _configValue = ConfigTasksFilePath $ xdgDataPath FP.</> "homodoro" FP.</> "tasks"}
            , _timerPopupAlertSetting = ConfigSetting{_configLabel = "Timer popup alert", _configValue = ConfigTimerPopupAlert True}
            , _timerAlertSoundVolumeSetting = ConfigSetting{_configLabel = "Timer alert sound volume", _configValue = ConfigTimerAlertSoundVolume 60}
            , _timerTickSoundVolumeSetting = ConfigSetting{_configLabel = "Timer tick sound volume", _configValue = ConfigTimerTickSoundVolume 60}
            , _audioDirectoryPathSetting = ConfigSetting{_configLabel = "Audio directory path", _configValue = ConfigAudioDirectoryPath $ xdgDataPath FP.</> "homodoro" FP.</> "audio"}
            }

createProgramFileAndDirectoriesIfNotExists :: IO ()
createProgramFileAndDirectoriesIfNotExists = do
    configFilePath <- xdgConfigFilePath
    D.createDirectoryIfMissing True (FP.takeDirectory configFilePath)
    configFileExists <- D.doesFileExist configFilePath
    defaultConfigFile <- defaultConfig
    unless configFileExists $ do
        writeFile configFilePath $ unpack $ encode defaultConfigFile
        createDirectoryIfMissing True $ configFilePathValue $ defaultConfigFile ^. audioDirectoryPathSetting
        createDirectoryIfMissing True (takeDirectory $ configFilePathValue $ defaultConfigFile ^. audioDirectoryPathSetting)
        let tasksFilePath = configFilePathValue $ defaultConfigFile ^. tasksFilePathSetting
        fileExists <- doesFileExist tasksFilePath
        unless fileExists $ do writeFile tasksFilePath ""

xdgConfigFilePath :: IO FilePath
xdgConfigFilePath = do
    xdgConfigPath <- D.getXdgDirectory D.XdgConfig ""
    pure $ xdgConfigPath FP.</> "homodoro" FP.</> "config"

updateConfig :: ConfigFileOperation -> IO ConfigFile
updateConfig ToggleStartStopSound = updateBoolConfig startStopSoundSetting
updateConfig ToggleTimerPopupAlert = updateBoolConfig timerPopupAlertSetting
updateConfig (AddInitialTimer timer time) = do
    configFile <- readConfig
    let updatedConfigFile = configFile & initialTimerL %~ addInitialTimer
    writeConfig updatedConfigFile
    return updatedConfigFile
  where
    addInitialTimer (ConfigInitialTimer _ val) = ConfigInitialTimer timer (max (val + time) 0)
    addInitialTimer val = val
    initialTimerL = case timer of
        Pomodoro -> pomodoroInitialTimerSetting . configValue
        ShortBreak -> shortBreakInitialTimerSetting . configValue
        LongBreak -> longBreakInitialTimerSetting . configValue
updateConfig (SetFilePathSetting settingL filePathSetting) = do
    configFile <- readConfig
    let updatedConfigFile = configFile & settingL . configValue .~ setConfigFilePath filePathSetting
    writeConfig updatedConfigFile
    return updatedConfigFile
  where
    setConfigFilePath (ConfigTasksFilePath val) = ConfigTasksFilePath val
    setConfigFilePath (ConfigAudioDirectoryPath val) = ConfigAudioDirectoryPath val
    setConfigFilePath val = val
updateConfig (AddSoundVolume settingL volStep) = do
    configFile <- readConfig
    let updatedConfigFile = configFile & settingL . configValue %~ addSoundVolumeStep
    writeConfig updatedConfigFile
    return updatedConfigFile
  where
    addSoundVolumeStep (ConfigTimerAlertSoundVolume val) = ConfigTimerAlertSoundVolume (min (max (val + volStep) 0) 100)
    addSoundVolumeStep (ConfigTimerTickSoundVolume val) = ConfigTimerTickSoundVolume (min (max (val + volStep) 0) 100)
    addSoundVolumeStep val = val

updateBoolConfig :: Lens' ConfigFile ConfigSetting -> IO ConfigFile
updateBoolConfig configL = do
    configFile <- readConfig
    let updatedConfigFile = configFile & configL . configValue %~ toggleBool
    writeConfig updatedConfigFile
    return updatedConfigFile

toggleBool :: ConfigSettingValue -> ConfigSettingValue
toggleBool (ConfigTimerStartStopSound b) = ConfigTimerStartStopSound (not b)
toggleBool (ConfigTimerPopupAlert b) = ConfigTimerPopupAlert (not b)
toggleBool val = val

writeConfig :: ConfigFile -> IO ()
writeConfig cfg = do
    configFilePath <- xdgConfigFilePath
    writeFile configFilePath $ unpack $ encode cfg

readConfig :: IO ConfigFile
readConfig = do
    configFilePath <- xdgConfigFilePath
    configFileContent <- readFile configFilePath
    maybe defaultConfig return (decode $ pack configFileContent)

readTasksFilePath :: IO FilePath
readTasksFilePath = do
    configFile <- readConfig
    return $ configFilePathValue $ configFile ^. tasksFilePathSetting

readAudioDirectoryPath :: IO FilePath
readAudioDirectoryPath = do
    configFile <- readConfig
    return $ configFilePathValue $ configFile ^. audioDirectoryPathSetting

defaultTasksFilePathIO :: IO FilePath
defaultTasksFilePathIO = do
    xdgDirectory <- D.getXdgDirectory D.XdgData ""
    return $ xdgDirectory FP.</> "homodoro" FP.</> "tasks.md"

readInitialTimer :: Timer -> IO Int
readInitialTimer timer = do
    initialTimerValue <$> readConfig
  where
    initialTimerValue configFile = case timer of
        Pomodoro -> configIntValue (configFile ^. pomodoroInitialTimerSetting)
        ShortBreak -> configIntValue (configFile ^. shortBreakInitialTimerSetting)
        LongBreak -> configIntValue (configFile ^. longBreakInitialTimerSetting)

readStartStopSound :: IO Bool
readStartStopSound = do
    configFile <- readConfig
    return $ configBoolValue $ configFile ^. startStopSoundSetting

readTimerPopupAlert :: IO Bool
readTimerPopupAlert = do
    configFile <- readConfig
    return $ configBoolValue $ configFile ^. timerPopupAlertSetting

readAlertSoundVolume :: IO Int
readAlertSoundVolume = do
    configFile <- readConfig
    return $ maybeConfigIntValue $ Just (configFile ^. timerAlertSoundVolumeSetting)

readTimerTickSoundVolume :: IO Int
readTimerTickSoundVolume = do
    configFile <- readConfig
    return $ maybeConfigIntValue $ Just (configFile ^. timerTickSoundVolumeSetting)

configFileSettings :: ConfigFile -> [ConfigSetting]
configFileSettings configFile =
    [ configFile ^. timerAlertSoundVolumeSetting
    , configFile ^. timerTickSoundVolumeSetting
    , configFile ^. pomodoroInitialTimerSetting
    , configFile ^. shortBreakInitialTimerSetting
    , configFile ^. longBreakInitialTimerSetting
    , configFile ^. startStopSoundSetting
    , configFile ^. timerPopupAlertSetting
    , configFile ^. tasksFilePathSetting
    , configFile ^. audioDirectoryPathSetting
    ]

findConfigSetting :: ConfigSettingValue -> [ConfigSetting] -> Maybe ConfigSetting
findConfigSetting configSettingValue =
    find
        ( \setting -> case (configSettingValue, setting ^. configValue) of
            (ConfigTimerAlertSoundVolume _, ConfigTimerAlertSoundVolume _) -> True
            (ConfigTimerTickSoundVolume _, ConfigTimerTickSoundVolume _) -> True
            (ConfigInitialTimer timer _, ConfigInitialTimer settingTimer _) -> timer == settingTimer
            (ConfigTimerPopupAlert _, ConfigTimerPopupAlert _) -> True
            _ -> setting ^. configValue == configSettingValue
        )

configSettingsValueToString :: ConfigSettingValue -> String
configSettingsValueToString (ConfigInitialTimer _ i) = formatTimer i
configSettingsValueToString (ConfigTimerStartStopSound b) = showBool b
configSettingsValueToString (ConfigTasksFilePath p) = show p
configSettingsValueToString (ConfigTimerPopupAlert b) = showBool b
configSettingsValueToString (ConfigTimerAlertSoundVolume vol) = show vol <> "%"
configSettingsValueToString (ConfigTimerTickSoundVolume vol) = show vol <> "%"
configSettingsValueToString (ConfigAudioDirectoryPath p) = show p

showBool :: Bool -> String
showBool true = if true then "Enabled" else "Disabled"

maybeConfigIntValue :: Maybe ConfigSetting -> Int
maybeConfigIntValue (Just (ConfigSetting _ (ConfigInitialTimer _ initialTimer))) = initialTimer
maybeConfigIntValue (Just (ConfigSetting _ (ConfigTimerAlertSoundVolume timerAlertSoundVolume))) = timerAlertSoundVolume
maybeConfigIntValue (Just (ConfigSetting _ (ConfigTimerTickSoundVolume timerTickSoundVolume))) = timerTickSoundVolume
maybeConfigIntValue _ = 0

configIntValue :: ConfigSetting -> Int
configIntValue (ConfigSetting _ (ConfigInitialTimer _ initialTimer)) = initialTimer
configIntValue (ConfigSetting _ (ConfigTimerAlertSoundVolume timerAlertSoundVolume)) = timerAlertSoundVolume
configIntValue (ConfigSetting _ (ConfigTimerTickSoundVolume timerTickSoundVolume)) = timerTickSoundVolume
configIntValue _ = 0

configFilePathValue :: ConfigSetting -> FilePath
configFilePathValue (ConfigSetting _ (ConfigTasksFilePath path)) = path
configFilePathValue (ConfigSetting _ (ConfigAudioDirectoryPath path)) = path
configFilePathValue _ = FP.empty

maybeConfigBoolValue :: Maybe ConfigSetting -> Bool
maybeConfigBoolValue (Just (ConfigSetting _ (ConfigTimerPopupAlert value))) = value
maybeConfigBoolValue (Just (ConfigSetting _ (ConfigTimerStartStopSound value))) = value
maybeConfigBoolValue _ = False

configBoolValue :: ConfigSetting -> Bool
configBoolValue (ConfigSetting _ (ConfigTimerPopupAlert value)) = value
configBoolValue (ConfigSetting _ (ConfigTimerStartStopSound value)) = value
configBoolValue _ = False
