{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config (..),
    createConfigFileIfNotExists,
    getTasksFilePath
)
where

import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson hiding ((.=))
import Data.Aeson.TH (deriveJSON)
import qualified Resources as R
import Control.Monad (unless)
import Control.Lens (makeLenses)
import Control.Lens.Getter ((^.))

data Config = Config
    { _pomodoroInitialTimer :: Int
    , _shortBreakInitialTimer :: Int
    , _longBreakInitialTimer :: Int
    , _activeTasksFilePath :: FilePath
    , _archivedTasksFilePath :: FilePath
    , _dailyTasksMode :: Bool --This will make tasks be grouped by date, just like the archived tasks
    }
deriveJSON defaultOptions ''Config
makeLenses ''Config

data ConfigFileOperation
    = ToggleDailyTasksMode
    | UpdateInitialTimer R.Timer Int
    | SetActiveTasksFilePath FilePath
    | SetArchivedTasksFilePath FilePath

type ConfigFileUpdate = ConfigFileOperation -> Config -> Config

defaultConfig :: IO Config
defaultConfig = do
    xdgDataPath <- D.getXdgDirectory D.XdgData ""
    return Config {
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

writeConfig :: Config -> IO ()
writeConfig cfg = do
    configFilePath <- getConfigFilePath
    BSL.writeFile configFilePath $ encode cfg

getConfig :: IO Config
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