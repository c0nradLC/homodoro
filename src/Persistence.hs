module Persistence (
    xdgPersistenceFilePath,
    defaultPersistence,
    writePersistence,
    readPersistence
    )
where
import Types (PersistenceFile (..), Timers (..), TimerState (..), Timer (Pomodoro))
import Data.Time.LocalTime (getZonedTime, LocalTime (localDay), ZonedTime (zonedTimeToLocalTime))
import qualified System.Directory as D
import qualified System.FilePath as FP
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.Aeson (encode, decode)

defaultPersistence :: (Int, Int, Int) -> IO PersistenceFile
defaultPersistence (pomodoroTimer, shortBreakTimer, longBreakTimer) = do
    zonedTime <- getZonedTime
    return
        PersistenceFile
            { _datePersisted = localDay (zonedTimeToLocalTime zonedTime)
            , _timersPersisted = Timers
                                    { _pomodoroState = TimerState {_timerCurrentValue = pomodoroTimer, _timerInitialValue = pomodoroTimer}
                                    , _shortBreakState = TimerState {_timerCurrentValue = shortBreakTimer, _timerInitialValue = shortBreakTimer}
                                    , _longBreakState = TimerState {_timerCurrentValue = longBreakTimer, _timerInitialValue = longBreakTimer}
                                    , _timerCurrentFocus = Pomodoro
                                    }
            , _pomodoroRoundsPersisted = 0
            , _focusedTimePersisted = 0
            , _breakTimePersisted = 0
            }

xdgPersistenceFilePath :: IO FilePath
xdgPersistenceFilePath = do
    xdgDataPath <- D.getXdgDirectory D.XdgData ""
    zonedTime <- getZonedTime
    pure $ xdgDataPath FP.</> "homodoro/data" FP.</> (show (localDay (zonedTimeToLocalTime zonedTime)) <> "_data.json")

writePersistence :: PersistenceFile -> IO ()
writePersistence pf = do
    persistenceFilePath <- xdgPersistenceFilePath
    writeFile persistenceFilePath $ unpack $ encode pf

readPersistence :: (Int, Int, Int) -> IO PersistenceFile
readPersistence initialTimerValues = do
    persistenceFilePath <- xdgPersistenceFilePath
    persistenceFileContent <- readFile persistenceFilePath
    maybe (defaultPersistence initialTimerValues) return (decode $ pack persistenceFileContent)