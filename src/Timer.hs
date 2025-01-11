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
import Resources (AppState, Audio (TimerEnded), Name (..), Timer (..), focus, longBreakTimer, pomodoroCounter, pomodoroCyclesCounter, pomodoroTimer, shortBreakTimer, timerRunning)
import Util (changeFocus)

tickTimer :: Maybe Name -> AppState -> EventM Name AppState ()
tickTimer currentFocus s = do
    case currentFocus of
        Just (TaskList Pomodoro) -> do
            tick pomodoroTimer (s ^. pomodoroTimer)
            when (s ^. pomodoroTimer == 0) $ do
                stopTimer
                alert "Pomodoro round ended!"
                finishRound pomodoroTimer Pomodoro
                pomodoroCounter .= (s ^. pomodoroCounter) + 1
                increasePomodoroCycleCounter s
        Just (TaskList ShortBreak) -> do
            tick shortBreakTimer (s ^. shortBreakTimer)
            when (s ^. shortBreakTimer == 0) $ do
                stopTimer
                alert "Short break ended!"
                finishRound shortBreakTimer ShortBreak
                changeFocus (TaskList Pomodoro) s
                focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
        Just (TaskList LongBreak) -> do
            tick longBreakTimer (s ^. longBreakTimer)
            when (s ^. longBreakTimer == 0) $ do
                stopTimer
                alert "Long break ended!"
                finishRound longBreakTimer LongBreak
                pomodoroCyclesCounter .= 0
                focus .= BF.focusSetCurrent (TaskList Pomodoro) (s ^. focus)
        _ -> return ()

tick :: Lens' AppState Int -> Int -> EventM Name AppState ()
tick timerL timerValue = timerL .= max (timerValue - 1) 0

stopTimer :: EventM Name AppState ()
stopTimer = timerRunning .= False

alert :: String -> EventM Name AppState ()
alert msg = do
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
