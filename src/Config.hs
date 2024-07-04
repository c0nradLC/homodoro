{-# LANGUAGE OverloadedStrings #-}

module Config (
    createConfigFileIfNotExists,
    getTasksFilePath,
    getInitialTimer
)
where

import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson hiding ((.=))
import qualified Resources as R
import Control.Monad (unless)
import Control.Lens.Getter ((^.))
import Resources (ConfigFile (..), ConfigFileOperation (..), ConfigFileUpdate, activeTasksFilePath, pomodoroInitialTimer, shortBreakInitialTimer, longBreakInitialTimer)
import Control.Monad.Cont (MonadIO(liftIO))

defaultConfig :: IO ConfigFile
defaultConfig = do
    xdgDataPath <- D.getXdgDirectory D.XdgData ""
    return ConfigFile {
      _pomodoroInitialTimer = 1500
    , _shortBreakInitialTimer = 300
    , _longBreakInitialTimer = 900
    , _activeTasksFilePath = xdgDataPath FP.</> "homodoro" FP.</> "tasks"
    , _archivedTasksFilePath = xdgDataPath FP.</> "homodoro" FP.</> "archived_tasks"
    , _dailyTasksMode = True
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
    homePath <- D.getXdgDirectory D.XdgConfig ""
    pure $ homePath FP.</> "homodoro" FP.</> "config"

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
        ToggleDailyTasksMode -> cfg {_dailyTasksMode = not $ _dailyTasksMode cfg}
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