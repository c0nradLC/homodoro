{-# LANGUAGE OverloadedStrings #-}

module Config (
    createConfigFileIfNotExists,
    getTasksFilePath,
    getInitialTimer,
    updateConfig,
    writeConfig,
    getConfig
)
where

import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as BSL
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigFileUpdate, activeTasksFilePath, longBreakInitialTimer, pomodoroInitialTimer, shortBreakInitialTimer)
import qualified Resources as R
import qualified System.Directory as D
import qualified System.FilePath as FP

defaultConfig :: IO ConfigFile
defaultConfig = do
    xdgDataPath <- D.getXdgDirectory D.XdgData ""
    return
        ConfigFile
            { _pomodoroInitialTimer = 1500
            , _shortBreakInitialTimer = 300
            , _longBreakInitialTimer = 900
            , _activeTasksFilePath = xdgDataPath FP.</> "homodoro" FP.</> "tasks"
            , _archivedTasksFilePath = xdgDataPath FP.</> "homodoro" FP.</> "archived_tasks"
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

writeConfig :: ConfigFile -> IO ()
writeConfig cfg = do
    configFilePath <- getConfigFilePath
    BSL.writeFile configFilePath $ encode cfg

getConfig :: IO ConfigFile
getConfig = do
    configFilePath <- getConfigFilePath
    configFileContent <- BSL.readFile configFilePath
    maybe defaultConfig return (decode configFileContent)

updateConfig :: ConfigFileUpdate
updateConfig cop cfg =
    case cop of
        UpdateInitialTimer timer time -> case timer of
            R.Pomodoro -> cfg  {_pomodoroInitialTimer = time}
            R.ShortBreak -> cfg  {_shortBreakInitialTimer = time}
            R.LongBreak -> cfg  {_longBreakInitialTimer = time}
        SetActiveTasksFilePath fp -> cfg {_activeTasksFilePath = fp}
        SetArchivedTasksFilePath fp -> cfg {_archivedTasksFilePath = fp}

getTasksFilePath :: IO FilePath
getTasksFilePath = do
    config <- getConfig
    return $ config ^. activeTasksFilePath

getInitialTimer :: R.Timer -> IO Int
getInitialTimer timer = do
    config <- getConfig
    case timer of
        R.Pomodoro -> return $ config ^. pomodoroInitialTimer
        R.ShortBreak -> return $ config ^. shortBreakInitialTimer
        R.LongBreak -> return $ config ^. longBreakInitialTimer
