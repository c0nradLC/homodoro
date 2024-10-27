{-# LANGUAGE OverloadedStrings #-}

module Config
  ( createConfigFileIfNotExists,
    getTasksFilePath,
    getInitialTimer,
    updateConfig,
    getConfig,
    extractFilePathValue,
    extractInitialTimerValue,
    extractStartStopSoundValue,
    configFileSettings
  )
where

import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as BSL
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigSetting (..), ConfigSettingValue(..), Timer(..), tasksFilePath, pomodoroInitialTimer, shortBreakInitialTimer, longBreakInitialTimer, startStopSound, configValue)
import qualified System.Directory as D
import qualified System.FilePath as FP
import Control.Lens ((&), (%~))
import qualified Control.Applicative as FP

defaultConfig :: IO ConfigFile
defaultConfig = do
  xdgDataPath <- D.getXdgDirectory D.XdgData ""
  return
    ConfigFile
      { _pomodoroInitialTimer = ConfigSetting { _configLabel = "Pomodoro round initial timer", _configValue = ConfigInitialTimer Pomodoro 1500 },
        _shortBreakInitialTimer = ConfigSetting { _configLabel = "Short break initial timer", _configValue = ConfigInitialTimer ShortBreak 300 },
        _longBreakInitialTimer = ConfigSetting { _configLabel = "Long break initial timer", _configValue = ConfigInitialTimer LongBreak 900 },
        _tasksFilePath = ConfigSetting { _configLabel = "Tasks file path", _configValue = ConfigTasksFilePath $ xdgDataPath FP.</> "homodoro" FP.</> "tasks" },
        _startStopSound = ConfigSetting { _configLabel = "Start/Stop sound", _configValue = ConfigStartStopSound False }
      }

createConfigFileIfNotExists :: IO ()
createConfigFileIfNotExists = do
  configFilePath <- getConfigFilePath
  D.createDirectoryIfMissing True (FP.takeDirectory configFilePath)
  fileExists <- D.doesFileExist configFilePath
  defaultConfigFile <- defaultConfig
  unless fileExists $ do BSL.writeFile configFilePath $ encode defaultConfigFile

getConfigFilePath :: IO FilePath
getConfigFilePath = do
  xdgConfigPath <- D.getXdgDirectory D.XdgConfig ""
  pure $ xdgConfigPath FP.</> "homodoro" FP.</> "config"

updateConfig :: ConfigFileOperation -> IO [ConfigSetting]
updateConfig (AddInitialTimer timer time) = do
  configFile <- getConfig
  case timer of
    Pomodoro -> do
      let updatedConfigFile = configFile & pomodoroInitialTimer . configValue %~ addInitialTimer
      writeConfig updatedConfigFile
      return $ configFileSettings updatedConfigFile
    ShortBreak -> do
      let updatedConfigFile = configFile & shortBreakInitialTimer . configValue %~ addInitialTimer
      writeConfig updatedConfigFile
      return $ configFileSettings updatedConfigFile
    LongBreak -> do
      let updatedConfigFile = configFile & longBreakInitialTimer . configValue %~ addInitialTimer
      writeConfig updatedConfigFile
      return $ configFileSettings updatedConfigFile
    where
      addInitialTimer (ConfigInitialTimer _ val) = ConfigInitialTimer timer (max (val + time) 0) 
      addInitialTimer val = val
updateConfig ToggleStartStopSound = do
  configFile <- getConfig
  let updatedConfigFile = configFile & startStopSound . configValue %~ toggleBool
  writeConfig updatedConfigFile 
  return $ configFileSettings updatedConfigFile
  where
    toggleBool (ConfigStartStopSound b) = ConfigStartStopSound (not b)
    toggleBool val = val

writeConfig :: ConfigFile -> IO ()
writeConfig cfg = do
  configFilePath <- getConfigFilePath
  BSL.writeFile configFilePath $ encode cfg

getConfig :: IO ConfigFile
getConfig = do
  configFilePath <- getConfigFilePath
  configFileContent <- BSL.readFile configFilePath
  maybe defaultConfig return (decode configFileContent)

getTasksFilePath :: IO FilePath
getTasksFilePath = do
  configFile <- getConfig
  let tasksFilePathSetting = configFile ^. tasksFilePath
  return $ extractFilePathValue tasksFilePathSetting

getInitialTimer :: Timer -> IO Int
getInitialTimer timer = do
  configFile <- getConfig
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

configFileSettings :: ConfigFile -> [ConfigSetting]
configFileSettings configFile =
  [ configFile ^. pomodoroInitialTimer
  , configFile ^. shortBreakInitialTimer
  , configFile ^. longBreakInitialTimer
  , configFile ^. tasksFilePath
  , configFile ^. startStopSound
  ]

extractInitialTimerValue :: ConfigSetting -> (Timer, Int)
extractInitialTimerValue (ConfigSetting _ (ConfigInitialTimer t initialTimer)) = (t, initialTimer)
extractInitialTimerValue _ = (Pomodoro, 0)

extractFilePathValue :: ConfigSetting -> FilePath
extractFilePathValue (ConfigSetting _ (ConfigTasksFilePath path)) = path
extractFilePathValue _ = FP.empty

extractStartStopSoundValue :: ConfigSetting -> Bool
extractStartStopSoundValue (ConfigSetting _ (ConfigStartStopSound value)) = value
extractStartStopSoundValue _ = False