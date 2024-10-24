{-# LANGUAGE OverloadedStrings #-}

module Config
  ( createConfigFileIfNotExists,
    getTasksFilePath,
    getInitialTimer,
    updateConfig,
    getConfig
  )
where

import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as BSL
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigFileUpdate, ConfigSetting (..), ConfigSettingValue(..), Timer(..), tasksFilePath, pomodoroInitialTimer, shortBreakInitialTimer, longBreakInitialTimer, startStopSound, extractInitialTimerValue, extractFilePathValue, configValue, getConfigFileSettings)
import qualified System.Directory as D
import qualified System.FilePath as FP
import Control.Lens ((&), (%~))

defaultConfig :: IO ConfigFile
defaultConfig = do
  xdgDataPath <- D.getXdgDirectory D.XdgData ""
  return
    ConfigFile
      { _pomodoroInitialTimer = ConfigSetting { _configLabel = "Pomodoro round initial timer", _configValue = ConfigInitialTimer 1500 },
        _shortBreakInitialTimer = ConfigSetting { _configLabel = "Short break initial timer", _configValue = ConfigInitialTimer 300 },
        _longBreakInitialTimer = ConfigSetting { _configLabel = "Long break initial timer", _configValue = ConfigInitialTimer 900 },
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

updateConfig :: ConfigFileUpdate
updateConfig (AddInitialTimer timer time) = do
  configFile <- getConfig
  case timer of
    Pomodoro -> do
      let updatedConfigFile = configFile & pomodoroInitialTimer . configValue %~ addInitialTimer
      writeConfig updatedConfigFile
      return $ getConfigFileSettings updatedConfigFile
    ShortBreak -> do
      let updatedConfigFile = configFile & shortBreakInitialTimer . configValue %~ addInitialTimer
      writeConfig updatedConfigFile
      return $ getConfigFileSettings updatedConfigFile
    LongBreak -> do
      let updatedConfigFile = configFile & longBreakInitialTimer . configValue %~ addInitialTimer
      writeConfig updatedConfigFile
      return $ getConfigFileSettings updatedConfigFile
  where
        addInitialTimer (ConfigInitialTimer val) = ConfigInitialTimer (max (val + time) 0) 
        addInitialTimer val = val
updateConfig ToggleStartStopSound = do
  configFile <- getConfig
  let updatedConfigFile = configFile & startStopSound . configValue %~ toggleBool
  writeConfig updatedConfigFile 
  return $ getConfigFileSettings updatedConfigFile where
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
    Pomodoro -> return $ extractInitialTimerValue pomodoroInitialTimerSetting
    ShortBreak -> return $ extractInitialTimerValue shortBreakInitialTimerSetting
    LongBreak -> return $ extractInitialTimerValue longBreakInitialTimerSetting