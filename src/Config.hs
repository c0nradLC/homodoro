{-# LANGUAGE OverloadedStrings #-}

module Config
  ( createConfigFileIfNotExists,
    readConfig,
    readTasksFilePath,
    readInitialTimer,
    readStartStopSound,
    readTickingSound,
    updateConfig,
    extractFilePathValue,
    extractInitialTimerValue,
    extractStartStopSoundValue,
    configFileSettings,
    configSettingsValueToText,
    initialTimerSetting
  )
where

import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as BSL
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigSetting (..), ConfigSettingValue(..), Timer(..), tasksFilePath, pomodoroInitialTimer, shortBreakInitialTimer, longBreakInitialTimer, startStopSound, configValue, tickingSound, Audio)
import qualified System.Directory as D
import qualified System.FilePath as FP
import Control.Lens ((&), (%~), (.~))
import qualified Control.Applicative as FP
import qualified Data.Text as T
import Data.List (find)
import UI.Timer (formatTimer)

defaultConfig :: IO ConfigFile
defaultConfig = do
  xdgDataPath <- D.getXdgDirectory D.XdgData ""
  return
    ConfigFile
      { _pomodoroInitialTimer = ConfigSetting { _configLabel = "Pomodoro round initial timer", _configValue = ConfigInitialTimer Pomodoro 1500 },
        _shortBreakInitialTimer = ConfigSetting { _configLabel = "Short break initial timer", _configValue = ConfigInitialTimer ShortBreak 300 },
        _longBreakInitialTimer = ConfigSetting { _configLabel = "Long break initial timer", _configValue = ConfigInitialTimer LongBreak 900 },
        _tasksFilePath = ConfigSetting { _configLabel = "Tasks file path", _configValue = ConfigTasksFilePath $ xdgDataPath FP.</> "homodoro" FP.</> "tasks" },
        _startStopSound = ConfigSetting { _configLabel = "Start/Stop sound", _configValue = ConfigStartStopSound False },
        _tickingSound = ConfigSetting { _configLabel = "Timer ticking sound", _configValue = ConfigTickingSound Nothing }
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
updateConfig (ChangeTickingSound mbTick) = do
  configFile <- readConfig
  let updatedConfigFile = configFile & tickingSound . configValue .~ ConfigTickingSound mbTick
  writeConfig updatedConfigFile
  return updatedConfigFile

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

readInitialTimer :: Timer -> IO Int
readInitialTimer timer = do
  configFile <- readConfig
  let pomodoroInitialTimerSetting   = configFile ^. pomodoroInitialTimer
      shortBreakInitialTimerSetting = configFile ^. shortBreakInitialTimer
      longBreakInitialTimerSetting  = configFile ^. longBreakInitialTimer
  case timer of
    Pomodoro -> let (_, initialTimer) = extractInitialTimerValue pomodoroInitialTimerSetting
                in return initialTimer
    ShortBreak -> let (_, initialTimer) = extractInitialTimerValue shortBreakInitialTimerSetting
                in return initialTimer
    LongBreak -> let (_, initialTimer) = extractInitialTimerValue longBreakInitialTimerSetting
                in return initialTimer

readStartStopSound :: IO Bool
readStartStopSound = do
  configFile <- readConfig
  return $ extractStartStopSoundValue $ configFile ^. startStopSound

readTickingSound :: IO (Maybe Audio)
readTickingSound = do
  configFile <- readConfig
  return $ extractTickingSoundValue $ configFile ^. tickingSound

configFileSettings :: ConfigFile -> [ConfigSetting]
configFileSettings configFile =
  [ configFile ^. pomodoroInitialTimer
  , configFile ^. shortBreakInitialTimer
  , configFile ^. longBreakInitialTimer
  , configFile ^. tasksFilePath
  , configFile ^. startStopSound
  , configFile ^. tickingSound
  ]

initialTimerSetting :: Timer -> [ConfigSetting] -> Maybe ConfigSetting
initialTimerSetting timer =
  find isInitialTimer 
  where
    isInitialTimer setting = case setting ^. configValue of
      ConfigInitialTimer cfgTimer _ -> timer == cfgTimer
      _                             -> False

configSettingsValueToText :: ConfigSettingValue -> T.Text
configSettingsValueToText (ConfigInitialTimer _ i) = T.pack $ formatTimer i
configSettingsValueToText (ConfigStartStopSound b) = T.pack $ show b
configSettingsValueToText (ConfigTasksFilePath p) = T.pack $ show p
configSettingsValueToText (ConfigTickingSound p) = T.pack $ show p

extractInitialTimerValue :: ConfigSetting -> (Timer, Int)
extractInitialTimerValue (ConfigSetting _ (ConfigInitialTimer t initialTimer)) = (t, initialTimer)
extractInitialTimerValue _ = (Pomodoro, 0)

extractFilePathValue :: ConfigSetting -> FilePath
extractFilePathValue (ConfigSetting _ (ConfigTasksFilePath path)) = path
extractFilePathValue _ = FP.empty

extractStartStopSoundValue :: ConfigSetting -> Bool
extractStartStopSoundValue (ConfigSetting _ (ConfigStartStopSound value)) = value
extractStartStopSoundValue _ = False

extractTickingSoundValue :: ConfigSetting -> Maybe Audio
extractTickingSoundValue (ConfigSetting _ (ConfigTickingSound tick)) = tick
extractTickingSoundValue _ = Nothing