{-# LANGUAGE OverloadedStrings #-}

module Config
  ( createConfigFileIfNotExists,
    getTasksFilePath,
    getInitialTimer,
    updateConfig,
    getConfig,
  )
where

import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as BSL
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigFileUpdate, longBreakInitialTimer, pomodoroInitialTimer, shortBreakInitialTimer, tasksFilePath)
import qualified Resources as R
import qualified System.Directory as D
import qualified System.FilePath as FP

defaultConfig :: IO ConfigFile
defaultConfig = do
  xdgDataPath <- D.getXdgDirectory D.XdgData ""
  return
    ConfigFile
      { _pomodoroInitialTimer = 1500,
        _shortBreakInitialTimer = 300,
        _longBreakInitialTimer = 900,
        _tasksFilePath = xdgDataPath FP.</> "homodoro" FP.</> "tasks"
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
updateConfig cop cfg = do
  configFilePath <- getConfigFilePath
  let config = case cop of
        UpdateInitialTimer timer time -> case timer of
          R.Pomodoro -> cfg {_pomodoroInitialTimer = max (_pomodoroInitialTimer cfg + time) 0}
          R.ShortBreak -> cfg {_shortBreakInitialTimer = max (_shortBreakInitialTimer cfg + time) 0}
          R.LongBreak -> cfg {_longBreakInitialTimer = max (_longBreakInitialTimer cfg + time) 0}
  BSL.writeFile configFilePath $ encode config

getConfig :: IO ConfigFile
getConfig = do
  configFilePath <- getConfigFilePath
  configFileContent <- BSL.readFile configFilePath
  maybe defaultConfig return (decode configFileContent)

getTasksFilePath :: IO FilePath
getTasksFilePath = do
  config <- getConfig
  return $ config ^. tasksFilePath

getInitialTimer :: R.Timer -> IO Int
getInitialTimer timer = do
  config <- getConfig
  case timer of
    R.Pomodoro -> return $ config ^. pomodoroInitialTimer
    R.ShortBreak -> return $ config ^. shortBreakInitialTimer
    R.LongBreak -> return $ config ^. longBreakInitialTimer
