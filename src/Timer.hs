{-# LANGUAGE RankNTypes #-}

module Timer (
tickTimer, stopTimer, alertTimerEnded, finishRound, increasePomodoroCycleCounter)
where

import Brick (EventM)
import qualified Config as CFG
import Control.Concurrent (forkIO)
import Control.Lens (Lens', uses, (.=), (^.))
import Control.Monad.Cont (MonadIO (liftIO))
import qualified Notify as NT
import Resources (AppState, Audio (TimerEnded), Name (..), Timer (..), pomodoroCyclesCounter, timerRunning)
import Util (changeFocus)

tickTimer :: Lens' AppState Int -> Int -> EventM Name AppState ()
tickTimer timerL timerValue = timerL .= max (timerValue - 1) 0

stopTimer :: EventM Name AppState ()
stopTimer = timerRunning .= False

alertTimerEnded :: String -> EventM Name AppState ()
alertTimerEnded msg = do
    _ <- liftIO $ forkIO $ NT.playAudio TimerEnded
    NT.alertRoundEnded msg

finishRound :: Lens' AppState Int -> Timer -> EventM Name AppState ()
finishRound timer currentTimer = do
    initialTimer <- liftIO $ CFG.readInitialTimer currentTimer
    timer .= initialTimer

increasePomodoroCycleCounter :: AppState -> EventM Name AppState ()
increasePomodoroCycleCounter s = do
    pomodoroCyclesCounter .= (s ^. pomodoroCyclesCounter) + 1
    updatedCycleCounter <- uses pomodoroCyclesCounter id
    if updatedCycleCounter == 4
        then do
            changeFocus (TaskList LongBreak) s
        else changeFocus (TaskList ShortBreak) s
