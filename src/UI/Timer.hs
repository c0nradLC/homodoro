{-# LANGUAGE OverloadedStrings #-}

module UI.Timer (
    drawTimers,
    formatTimer,
)
where

import Brick (Widget, padLeftRight, padTopBottom, str, withAttr, (<+>), (<=>))
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import Text.Printf (printf)
import Types (AppState, Name, Timer (LongBreak, Pomodoro, ShortBreak), longBreakState, pomodoroCounter, pomodoroState, shortBreakState, timerCurrentValue, timerRunning, timers, timerCurrentFocus)
import UI.Attributes (selectedTimerAttr, timerAttr)

drawTimers :: AppState -> Widget Name
drawTimers s =
    case s ^. timers . timerCurrentFocus of
        Pomodoro ->
            B.border $
                C.hCenter
                    ( withAttr selectedTimerAttr (label "Pomodoro")
                        <+> padLeftRight 2 (label "Short break")
                        <+> label "Long break"
                    )
                    <=> drawTimerAndPomodoroCounter (s ^. timers . pomodoroState ^. timerCurrentValue)
        ShortBreak ->
            B.border $
                C.hCenter
                    ( label "Pomodoro"
                        <+> padLeftRight 2 (withAttr selectedTimerAttr $ label "Short break")
                        <+> label "Long break"
                    )
                    <=> drawTimerAndPomodoroCounter (s ^. timers . shortBreakState ^. timerCurrentValue)
        LongBreak ->
            B.border $
                C.hCenter
                    ( label "Pomodoro"
                        <+> padLeftRight 2 (label "Short break")
                        <+> withAttr selectedTimerAttr (label "Long break")
                    )
                    <=> drawTimerAndPomodoroCounter (s ^. timers . longBreakState ^. timerCurrentValue)
  where
    drawTimerAndPomodoroCounter timerValue =
        drawTimer (s ^. timerRunning) timerValue
            <=> drawPomodorosCounter s

label :: String -> Widget Name
label s = B.border $ padLeftRight 1 $ str s

drawTimer :: Bool -> Int -> Widget Name
drawTimer running timerValue =
    C.hCenter $
        padTopBottom 1 $
            if running
                then withAttr timerAttr timerWidget
                else timerWidget
  where
    timerWidget =
        padTopBottom 1 $
            padLeftRight 1 $
                str $
                    formatTimer timerValue

drawPomodorosCounter :: AppState -> Widget Name
drawPomodorosCounter s = C.hCenter (label (formatPomodoroCounter (s ^. pomodoroCounter)))

formatTimer :: Int -> String
formatTimer timer =
    let minutes = timer `div` 60
        seconds = timer `mod` 60
     in printf "%02d:%02d" minutes seconds

formatPomodoroCounter :: Int -> String
formatPomodoroCounter =
    printf "Pomodoros: %01d"
