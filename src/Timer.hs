{-# LANGUAGE RankNTypes #-}

module Timer (
    tickTimer,
)
where

import Brick (EventM)
import qualified Brick.Focus as BF
import qualified Config as CFG
import Control.Concurrent (forkIO)
import Control.Lens (Lens', uses, (.=), (^.))
import Control.Monad (when)
import Control.Monad.Cont (MonadIO (liftIO))
import qualified Notify as NT
import Resources (AppState, Name (..), Timer (..), focus, longBreakTimer, pomodoroCycleCounter, pomodoroTimer, shortBreakTimer, timerRunning)

stopTimerAndAlert :: String -> EventM Name AppState ()
stopTimerAndAlert msg = do
    timerRunning .= False
    _ <- liftIO $ forkIO NT.playAlertSound
    NT.alertRoundEnded msg

finishRound :: Lens' AppState Int -> Timer -> EventM Name AppState ()
finishRound timer currentTimer = do
    initialTimer <- liftIO $ CFG.getInitialTimer currentTimer
    timer .= initialTimer

tickTimer :: AppState -> EventM Name AppState ()
tickTimer s
    | s ^. timerRunning = case BF.focusGetCurrent (s ^. focus) of
        Just (TaskList Pomodoro) -> do
            pomodoroTimer .= max ((s ^. pomodoroTimer) - 1) 0
            when (s ^. pomodoroTimer == 0) $ do
                stopTimerAndAlert "Pomodoro round ended!"
                finishRound pomodoroTimer Pomodoro
                focus .= BF.focusSetCurrent (TaskList ShortBreak) (s ^. focus)
        Just (TaskList ShortBreak) -> do
            shortBreakTimer .= max ((s ^. shortBreakTimer) - 1) 0
            when (s ^. shortBreakTimer == 0) $ do
                stopTimerAndAlert "Short break ended!"
                finishRound shortBreakTimer ShortBreak
                pomodoroCycleCounter .= (s ^. pomodoroCycleCounter) + 1
                updatedCycleCounter <- uses pomodoroCycleCounter id
                if updatedCycleCounter == 4
                    then do
                        focus .= BF.focusSetCurrent (TaskList LongBreak) (s ^. focus)
                    else focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
        Just (TaskList LongBreak) -> do
            longBreakTimer .= max ((s ^. longBreakTimer) - 1) 0
            when (s ^. longBreakTimer == 0) $ do
                stopTimerAndAlert "Long break ended!"
                finishRound longBreakTimer LongBreak
                focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
        _ -> return ()
    | otherwise = return ()
