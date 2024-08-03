module UI.Timer
  ( drawTimers,
  )
where

import Brick (Widget, padLeftRight, padTopBottom, str, withAttr, (<+>), (<=>))
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import Resources (AppState, Name (TaskList), Timer (LongBreak, Pomodoro, ShortBreak), focus, longBreakTimer, pomodoroCounter, pomodoroTimer, shortBreakTimer)
import Text.Printf (printf)
import UI.Attributes (selectedTimerAttr, timerAttr)

drawTimers :: AppState -> Widget Name
drawTimers s =
  case BF.focusGetCurrent (s ^. focus) of
    Just (TaskList timer) -> case timer of
      Pomodoro ->
        (C.hCenter (withAttr selectedTimerAttr (label "Pomodoro") <+> padLeftRight 2 (label "Short break") <+> label "Long break") <=>) $
          drawTimer (s ^. pomodoroTimer)
            <=> drawPomodorosCounter s
      ShortBreak ->
        (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (withAttr selectedTimerAttr $ label "Short break") <+> label "Long break") <=>) $
          drawTimer (s ^. shortBreakTimer)
            <=> drawPomodorosCounter s
      LongBreak ->
        (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (label "Short break") <+> withAttr selectedTimerAttr (label "Long break")) <=>) $
          drawTimer (s ^. longBreakTimer)
            <=> drawPomodorosCounter s
    _ ->
      (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (label "Short break") <+> label "Long break") <=>) $
        drawTimer (s ^. pomodoroTimer)
          <=> drawPomodorosCounter s

label :: String -> Widget Name
label s = B.border $ padLeftRight 1 $ str s

drawTimer :: Int -> Widget Name
drawTimer timerDuration =
  C.hCenter $
    padTopBottom 2 $
      withAttr timerAttr $
        padTopBottom 1 $
          padLeftRight 1 $
            str $
              formatTimer timerDuration

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
