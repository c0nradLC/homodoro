{-# LANGUAGE OverloadedStrings #-}

module Config (
    createConfigFileIfNotExists,
    readConfig,
    readTasksFilePath,
    readInitialTimer,
    readStartStopSound,
    updateConfig,
    extractFilePathValue,
    extractInitialTimerValue,
    extractStartStopSoundValue,
    configFileSettings,
    configSettingsValueToText,
    findInitialTimerSetting,
    defaultTasksFilePathIO,
)
where

import qualified Control.Applicative as FP
import Control.Lens ((%~), (&))
import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as BSL
import Data.List (find)
import qualified Data.Text as T
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigSetting (..), ConfigSettingValue (..), Timer (..), configValue, longBreakInitialTimer, pomodoroInitialTimer, shortBreakInitialTimer, startStopSound, tasksFilePath)
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
            , _startStopSound = ConfigSetting{_configLabel = "Start/Stop sound", _configValue = ConfigStartStopSound False}
            , _tasksFilePath = ConfigSetting{_configLabel = "Tasks file path", _configValue = ConfigTasksFilePath $ xdgDataPath FP.</> "homodoro" FP.</> "tasks"}
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
  where
    toggleBool (ConfigStartStopSound b) = ConfigStartStopSound (not b)
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
    return $ extractFilePathValue $ configFile ^. tasksFilePath

defaultTasksFilePathIO :: IO FilePath
defaultTasksFilePathIO = do
    xdgDirectory <- D.getXdgDirectory D.XdgData ""
    return $ xdgDirectory FP.</> "homodoro" FP.</> "tasks"

readInitialTimer :: Timer -> IO Int
readInitialTimer timer = do
    configFile <- readConfig
    let initialTimerSetting = findInitialTimerSetting timer $ configFileSettings configFile
    return $ extractInitialTimerValue initialTimerSetting

readStartStopSound :: IO Bool
readStartStopSound = do
    configFile <- readConfig
    return $ extractStartStopSoundValue $ configFile ^. startStopSound

configFileSettings :: ConfigFile -> [ConfigSetting]
configFileSettings configFile =
    [ configFile ^. pomodoroInitialTimer
    , configFile ^. shortBreakInitialTimer
    , configFile ^. longBreakInitialTimer
    , configFile ^. tasksFilePath
    , configFile ^. startStopSound
    ]

findInitialTimerSetting :: Timer -> [ConfigSetting] -> Maybe ConfigSetting
findInitialTimerSetting timer =
    find isInitialTimer
  where
    isInitialTimer setting = case setting ^. configValue of
        ConfigInitialTimer cfgTimer _ -> timer == cfgTimer
        _ -> False

configSettingsValueToText :: ConfigSettingValue -> T.Text
configSettingsValueToText (ConfigInitialTimer _ i) = T.pack $ formatTimer i
configSettingsValueToText (ConfigStartStopSound b) = T.pack $ show b
configSettingsValueToText (ConfigTasksFilePath p) = T.pack $ show p

extractInitialTimerValue :: Maybe ConfigSetting -> Int
extractInitialTimerValue (Just (ConfigSetting _ (ConfigInitialTimer _ initialTimer))) = initialTimer
extractInitialTimerValue _ = 0

extractFilePathValue :: ConfigSetting -> FilePath
extractFilePathValue (ConfigSetting _ (ConfigTasksFilePath path)) = path
extractFilePathValue _ = FP.empty

extractStartStopSoundValue :: ConfigSetting -> Bool
extractStartStopSoundValue (ConfigSetting _ (ConfigStartStopSound value)) = value
extractStartStopSoundValue _ = False
