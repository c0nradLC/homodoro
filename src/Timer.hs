module Timer
(
 tickTimer
)
where
import Resources (AppState, Name (..), Timer (..), timerRunning, focus, pomodoroTimer, shortBreakTimer, longBreakTimer, pomodoroCycleCounter)
import Brick (EventM)
import qualified Brick.Focus as BF
import Control.Lens ((^.), (.=), uses)
import Control.Monad (when)
import qualified Notify as NT
import Control.Concurrent (forkIO)
import Control.Monad.Cont (MonadIO(liftIO))
import qualified Config as CFG

stopTimerAndAlert :: String -> EventM Name AppState ()
stopTimerAndAlert msg = do
    timerRunning .= False
    _ <- liftIO $ forkIO NT.playAlertSound
    NT.alertRoundEnded msg

finishRound :: AppState -> Name -> EventM Name AppState ()
finishRound s currentFocus = do
    case currentFocus of
        TaskList Pomodoro -> do
                initialTimer <- liftIO $ CFG.getInitialTimer Pomodoro
                pomodoroTimer .= initialTimer
                stopTimerAndAlert "Pomodoro round ended!"
                focus .= BF.focusSetCurrent (TaskList ShortBreak) (s ^. focus)
        TaskList ShortBreak -> do
                initialTimer <- liftIO $ CFG.getInitialTimer ShortBreak
                shortBreakTimer .= initialTimer
                stopTimerAndAlert "Short break ended!"
                pomodoroCycleCounter .= (s ^. pomodoroCycleCounter) + 1
                updatedCycleCounter <- uses pomodoroCycleCounter id
                if updatedCycleCounter == 4
                    then do
                        focus .= BF.focusSetCurrent (TaskList LongBreak) (s ^. focus)
                    else focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
        TaskList LongBreak -> do
                initialTimer <- liftIO $ CFG.getInitialTimer LongBreak
                longBreakTimer .= initialTimer
                stopTimerAndAlert "Long break ended!"
                focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                pomodoroCycleCounter .= 0
        _ -> return ()

tickTimer :: AppState -> EventM Name AppState ()
tickTimer s
    | s ^. timerRunning = case BF.focusGetCurrent (s ^. focus) of
        Just cfs@(TaskList Pomodoro) -> do
            pomodoroTimer .= max ((s ^. pomodoroTimer) - 1) 0
            when (s ^. pomodoroTimer == 0) $ do
                finishRound s cfs
        Just cfs@(TaskList ShortBreak) -> do
            shortBreakTimer .= max ((s ^. shortBreakTimer) - 1) 0
            when (s ^. shortBreakTimer == 0) $ do
                finishRound s cfs
        Just cfs@(TaskList LongBreak) -> do
            longBreakTimer .= max ((s ^. longBreakTimer) - 1) 0
            when (s ^. longBreakTimer == 0) $ do
                finishRound s cfs
        _ -> return ()
    | otherwise = return ()