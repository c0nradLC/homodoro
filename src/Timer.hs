module Timer
(
 tickTimer
)
where
import Resources (AppState, Name (TaskList), Timer (Pomodoro, ShortBreak, LongBreak), timerRunning, focus, pomodoroTimer, pomodoroInitialTimer, shortBreakTimer, shortBreakInitialTimer, longBreakTimer, longBreakInitialTimer, pomodoroCycleCounter)
import Brick (EventM)
import qualified Brick.Focus as BF
import Control.Lens ((^.), (.=), uses)
import Control.Monad (when)
import qualified Notify as NT
import Control.Concurrent (forkIO)
import Control.Monad.Cont (MonadIO(liftIO))

stopTimerAndAlert :: String -> EventM Name AppState ()
stopTimerAndAlert msg = do
    timerRunning .= False
    _ <- liftIO $ forkIO NT.playAlertSound
    NT.alertRoundEnded msg

finishShortBreakRound :: AppState -> EventM Name AppState ()
finishShortBreakRound s = do
    pomodoroCycleCounter .= (s ^. pomodoroCycleCounter) + 1
    updatedCycleCounter <- uses pomodoroCycleCounter id
    if updatedCycleCounter == 4
        then do
            focus .= BF.focusSetCurrent (TaskList LongBreak) (s ^. focus)
        else focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)

tickTimer :: AppState -> EventM Name AppState ()
tickTimer s
    | s ^. timerRunning = case BF.focusGetCurrent (s ^. focus) of
        Just (TaskList Pomodoro) -> do
            pomodoroTimer .= max ((s ^. pomodoroTimer) - 1) 0
            when (s ^. pomodoroTimer == 0) $ do
                pomodoroTimer .= s ^. pomodoroInitialTimer
                stopTimerAndAlert "Pomodoro round ended!"
                focus .= BF.focusSetCurrent (TaskList ShortBreak) (s ^. focus)
        Just (TaskList ShortBreak) -> do
            shortBreakTimer .= max ((s ^. shortBreakTimer) - 1) 0
            when (s ^. shortBreakTimer == 0) $ do
                shortBreakTimer .= s ^. shortBreakInitialTimer
                stopTimerAndAlert "Short break ended!"
                finishShortBreakRound s
        Just (TaskList LongBreak) -> do
            longBreakTimer .= max ((s ^. longBreakTimer) - 1) 0
            when (s ^. longBreakTimer == 0) $ do
                longBreakTimer .= s ^. longBreakInitialTimer
                stopTimerAndAlert "Long break ended!"
                focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
                pomodoroCycleCounter .= 0
        _ -> return ()
    | otherwise = return ()