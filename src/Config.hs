{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Config (
    createConfigFileIfNotExists,
    readConfig,
    readTasksFilePath,
    readInitialTimer,
    readStartStopSound,
    readTimerPopupAlert,
    readAlertSoundVolume,
    readTimerTickSoundVolume,
    updateConfig,
    configFileSettings,
    configSettingsValueToText,
    defaultTasksFilePathIO,
    maybeConfigIntValue,
    maybeConfigBoolValue,
    configBoolValue,
    findConfigSetting,
    showBool,
    soundVolumePercentage,
)
where

import qualified Control.Applicative as FP
import Control.Lens (Lens', (%~), (&), (.~))
import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.List (find)
import qualified Data.Text as T
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigSetting (..), ConfigSettingValue (..), Timer (..), timerAlertSoundVolume, configValue, longBreakInitialTimer, pomodoroInitialTimer, shortBreakInitialTimer, startStopSound, tasksFilePath, timerPopupAlert, timerTickSoundVolume)
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
            , _timerPopupAlert = ConfigSetting{_configLabel = "Timer popup alert", _configValue = ConfigTimerPopupAlert True}
            , _timerAlertSoundVolume = ConfigSetting{_configLabel = "Timer alert sound volume", _configValue = ConfigTimerAlertSoundVolume 60}
            , _timerTickSoundVolume = ConfigSetting{_configLabel = "Timer tick sound volume", _configValue = ConfigTimerTickSoundVolume 60}
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
updateConfig ToggleStartStopSound = updateBoolConfig startStopSound
updateConfig ToggleTimerPopupAlert = updateBoolConfig timerPopupAlert
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
updateConfig (SetTasksFilePath fp) = do
    configFile <- readConfig
    let updatedConfigFile = configFile & tasksFilePath . configValue .~ ConfigTasksFilePath fp
    writeConfig updatedConfigFile
    return updatedConfigFile
updateConfig (AddSoundVolume alertSoundVolumeL volStep) = do
    configFile <- readConfig
    let updatedConfigFile = configFile & alertSoundVolumeL . configValue %~ addSoundVolumeStep
    writeConfig updatedConfigFile
    return updatedConfigFile
  where
    addSoundVolumeStep (ConfigTimerAlertSoundVolume val) = ConfigTimerAlertSoundVolume (min (max (val + volStep) 0) 128)
    addSoundVolumeStep (ConfigTimerTickSoundVolume val) = ConfigTimerTickSoundVolume (min (max (val + volStep) 0) 128)
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
  where
    initialTimerValue configFile = case timer of
        Pomodoro -> configIntValue (configFile ^. pomodoroInitialTimer)
        ShortBreak -> configIntValue (configFile ^. shortBreakInitialTimer)
        LongBreak -> configIntValue (configFile ^. longBreakInitialTimer)

readStartStopSound :: IO Bool
readStartStopSound = do
    configFile <- readConfig
    return $ configBoolValue $ configFile ^. startStopSound

readTimerPopupAlert :: IO Bool
readTimerPopupAlert = do
    configFile <- readConfig
    return $ configBoolValue $ configFile ^. timerPopupAlert

readAlertSoundVolume :: IO Int
readAlertSoundVolume = do
    configFile <- readConfig
    return $ maybeConfigIntValue $ Just (configFile ^. timerAlertSoundVolume)

readTimerTickSoundVolume :: IO Int
readTimerTickSoundVolume = do
    configFile <- readConfig
    return $ maybeConfigIntValue $ Just (configFile ^. timerTickSoundVolume)

configFileSettings :: ConfigFile -> [ConfigSetting]
configFileSettings configFile =
    [ configFile ^. timerAlertSoundVolume
    , configFile ^. timerTickSoundVolume
    , configFile ^. pomodoroInitialTimer
    , configFile ^. shortBreakInitialTimer
    , configFile ^. longBreakInitialTimer
    , configFile ^. startStopSound
    , configFile ^. timerPopupAlert
    , configFile ^. tasksFilePath
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

configSettingsValueToText :: ConfigSettingValue -> T.Text
configSettingsValueToText (ConfigInitialTimer _ i) = T.pack $ formatTimer i
configSettingsValueToText (ConfigTimerStartStopSound b) = T.pack $ showBool b
configSettingsValueToText (ConfigTasksFilePath p) = T.pack $ show p
configSettingsValueToText (ConfigTimerPopupAlert b) = T.pack $ showBool b
configSettingsValueToText (ConfigTimerAlertSoundVolume vol) = T.pack $ soundVolumePercentage vol
configSettingsValueToText (ConfigTimerTickSoundVolume vol) = T.pack $ soundVolumePercentage vol

showBool :: Bool -> String
showBool true = if true then "Enabled" else "Disabled"

maybeConfigIntValue :: Maybe ConfigSetting -> Int
maybeConfigIntValue (Just (ConfigSetting _ (ConfigInitialTimer _ initialTimer))) = initialTimer
maybeConfigIntValue (Just (ConfigSetting _ (ConfigTimerAlertSoundVolume currentAlertSoundVolume))) = currentAlertSoundVolume
maybeConfigIntValue (Just (ConfigSetting _ (ConfigTimerTickSoundVolume currentTimerTickSoundVolume))) = currentTimerTickSoundVolume
maybeConfigIntValue _ = 0

configIntValue :: ConfigSetting -> Int
configIntValue (ConfigSetting _ (ConfigInitialTimer _ initialTimer)) = initialTimer
configIntValue (ConfigSetting _ (ConfigTimerAlertSoundVolume currentAlertSoundVolume)) = currentAlertSoundVolume
configIntValue (ConfigSetting _ (ConfigTimerTickSoundVolume currentTimerTickSoundVolume)) = currentTimerTickSoundVolume
configIntValue _ = 0

configFilePathValue :: ConfigSetting -> FilePath
configFilePathValue (ConfigSetting _ (ConfigTasksFilePath path)) = path
configFilePathValue _ = FP.empty

maybeConfigBoolValue :: Maybe ConfigSetting -> Bool
maybeConfigBoolValue (Just (ConfigSetting _ (ConfigTimerPopupAlert value))) = value
maybeConfigBoolValue (Just (ConfigSetting _ (ConfigTimerStartStopSound value))) = value
maybeConfigBoolValue _ = False

configBoolValue :: ConfigSetting -> Bool
configBoolValue (ConfigSetting _ (ConfigTimerPopupAlert value)) = value
configBoolValue (ConfigSetting _ (ConfigTimerStartStopSound value)) = value
configBoolValue _ = False

soundVolumePercentage :: Int -> String
soundVolumePercentage vol = show (round ((fromIntegral vol / 128 :: Double) * 100) :: Int) ++ "%"
