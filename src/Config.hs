{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Config (
    readConfigFile,
    readTasksFilePath,
    updateConfig,
    configFileSettings,
    configSettingsValueToString,
    configBoolValue,
    findConfigSetting,
    showBool,
    configFilePathValue,
    initialTimerSettingL,
    configIntValue,
    soundVolumePercentage,
    xdgConfigFilePath,
    defaultConfig
)
where

import qualified Control.Applicative as FP
import Control.Lens (Lens', (&), (.~))
import Control.Lens.Getter ((^.))
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (find)
import qualified System.Directory as D
import qualified System.FilePath as FP
import Types (ConfigFile (..), ConfigSetting (..), ConfigSettingValue (..), Timer (..), audioDirectoryPathSetting, configValue, longBreakInitialTimerSetting, pomodoroInitialTimerSetting, shortBreakInitialTimerSetting, tasksFilePathSetting, timerAlertSoundVolumeSetting, timerPopupAlertSetting, timerStartStopSoundVolumeSetting, timerTickSoundVolumeSetting)
import UI.Timer (formatTimer)

defaultConfig :: IO ConfigFile
defaultConfig = do
    xdgDataPath <- D.getXdgDirectory D.XdgData ""
    return
        ConfigFile
            { _pomodoroInitialTimerSetting = ConfigSetting{_configLabel = "Pomodoro round initial timer", _configValue = ConfigInitialTimer Pomodoro 1500}
            , _shortBreakInitialTimerSetting = ConfigSetting{_configLabel = "Short break initial timer", _configValue = ConfigInitialTimer ShortBreak 300}
            , _longBreakInitialTimerSetting = ConfigSetting{_configLabel = "Long break initial timer", _configValue = ConfigInitialTimer LongBreak 900}
            , _timerStartStopSoundVolumeSetting = ConfigSetting{_configLabel = "Timer start/stop sound volume", _configValue = ConfigTimerStartStopSoundVolume 60}
            , _tasksFilePathSetting = ConfigSetting{_configLabel = "Tasks file path", _configValue = ConfigTasksFilePath $ xdgDataPath FP.</> "homodoro" FP.</> "tasks.md"}
            , _timerPopupAlertSetting = ConfigSetting{_configLabel = "Timer popup alert", _configValue = ConfigTimerPopupAlert True}
            , _timerAlertSoundVolumeSetting = ConfigSetting{_configLabel = "Timer alert sound volume", _configValue = ConfigTimerAlertSoundVolume 60}
            , _timerTickSoundVolumeSetting = ConfigSetting{_configLabel = "Timer tick sound volume", _configValue = ConfigTimerTickSoundVolume 60}
            , _audioDirectoryPathSetting = ConfigSetting{_configLabel = "Audio directory path", _configValue = ConfigAudioDirectoryPath $ xdgDataPath FP.</> "homodoro" FP.</> "audio"}
            }


xdgConfigFilePath :: IO FilePath
xdgConfigFilePath = do
    xdgConfigPath <- D.getXdgDirectory D.XdgConfig ""
    pure $ xdgConfigPath FP.</> "homodoro" FP.</> "config"

updateConfig :: ConfigFile -> Lens' ConfigFile ConfigSetting -> ConfigSettingValue -> IO ConfigFile
updateConfig configFile settingL newSettingValue = do
    let updatedConfigFile = configFile & settingL . configValue .~ newSettingValue
    writeConfig updatedConfigFile
    return updatedConfigFile

writeConfig :: ConfigFile -> IO ()
writeConfig cfg = do
    configFilePath <- xdgConfigFilePath
    writeFile configFilePath $ unpack $ encode cfg

readConfigFile :: IO ConfigFile
readConfigFile = do
    configFilePath <- xdgConfigFilePath
    configFileContent <- readFile configFilePath
    maybe defaultConfig return (decode $ pack configFileContent)

readTasksFilePath :: IO FilePath
readTasksFilePath = do
    configFile <- readConfigFile
    return $ configFilePathValue $ configFile ^. tasksFilePathSetting

initialTimerSettingL :: Timer -> Lens' ConfigFile ConfigSetting
initialTimerSettingL timer = case timer of
    Pomodoro -> pomodoroInitialTimerSetting
    ShortBreak -> shortBreakInitialTimerSetting
    LongBreak -> longBreakInitialTimerSetting

configFileSettings :: ConfigFile -> [ConfigSetting]
configFileSettings configFile =
    [ configFile ^. timerTickSoundVolumeSetting
    , configFile ^. timerAlertSoundVolumeSetting
    , configFile ^. timerStartStopSoundVolumeSetting
    , configFile ^. pomodoroInitialTimerSetting
    , configFile ^. shortBreakInitialTimerSetting
    , configFile ^. longBreakInitialTimerSetting
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
            (ConfigTimerStartStopSoundVolume _, ConfigTimerStartStopSoundVolume _) -> True
            (ConfigInitialTimer timer _, ConfigInitialTimer settingTimer _) -> timer == settingTimer
            (ConfigTimerPopupAlert _, ConfigTimerPopupAlert _) -> True
            _ -> setting ^. configValue == configSettingValue
        )

configSettingsValueToString :: ConfigSettingValue -> String
configSettingsValueToString (ConfigInitialTimer _ i) = formatTimer i
configSettingsValueToString (ConfigTasksFilePath p) = show p
configSettingsValueToString (ConfigTimerPopupAlert b) = showBool b
configSettingsValueToString (ConfigTimerAlertSoundVolume vol) = soundVolumePercentage vol
configSettingsValueToString (ConfigTimerTickSoundVolume vol) = soundVolumePercentage vol
configSettingsValueToString (ConfigTimerStartStopSoundVolume vol) = soundVolumePercentage vol
configSettingsValueToString (ConfigAudioDirectoryPath p) = show p

showBool :: Bool -> String
showBool true = if true then "Enabled" else "Disabled"

soundVolumePercentage :: Int -> String
soundVolumePercentage vol = show (round ((fromIntegral vol / 128 :: Double) * 100) :: Int) ++ "%"

configIntValue :: ConfigSetting -> Int
configIntValue (ConfigSetting _ (ConfigInitialTimer _ initialTimer)) = initialTimer
configIntValue (ConfigSetting _ (ConfigTimerAlertSoundVolume timerAlertSoundVolume)) = timerAlertSoundVolume
configIntValue (ConfigSetting _ (ConfigTimerTickSoundVolume timerTickSoundVolume)) = timerTickSoundVolume
configIntValue (ConfigSetting _ (ConfigTimerStartStopSoundVolume timerStartStopSoundVolume)) = timerStartStopSoundVolume
configIntValue _ = 0

configFilePathValue :: ConfigSetting -> FilePath
configFilePathValue (ConfigSetting _ (ConfigTasksFilePath path)) = path
configFilePathValue (ConfigSetting _ (ConfigAudioDirectoryPath path)) = path
configFilePathValue _ = FP.empty

configBoolValue :: ConfigSetting -> Bool
configBoolValue (ConfigSetting _ (ConfigTimerPopupAlert value)) = value
configBoolValue _ = False
