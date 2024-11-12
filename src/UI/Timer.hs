module UI.Timer
  ( drawTimers
  , formatTimer
  )
where

import Brick (Widget, padLeftRight, padTopBottom, str, withAttr, (<+>), (<=>))
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import Resources (AppState, Name (TaskList), Timer (LongBreak, Pomodoro, ShortBreak), focus, longBreakTimer, pomodoroCounter, pomodoroTimer, shortBreakTimer, timerRunning)
import Text.Printf (printf)
import UI.Attributes (selectedTimerAttr, timerAttr)

drawTimers :: AppState -> Widget Name
drawTimers s =
  case BF.focusGetCurrent (s ^. focus) of
    Just (TaskList timer) -> case timer of
      Pomodoro ->
        B.border $ C.hCenter
        (  withAttr selectedTimerAttr (label "Pomodoro")
           <+> padLeftRight 2 (label "Short break")
           <+> label "Long break")
           <=> timerAndPomodoroCounter pomodoroTimer
      ShortBreak ->
        B.border $ C.hCenter 
          ( label "Pomodoro"
            <+> padLeftRight 2 (withAttr selectedTimerAttr $ label "Short break")
            <+> label "Long break")
        <=> timerAndPomodoroCounter shortBreakTimer
      LongBreak ->
        B.border $ C.hCenter
          ( label "Pomodoro"
            <+> padLeftRight 2 (label "Short break")
            <+> withAttr selectedTimerAttr (label "Long break"))
            <=> timerAndPomodoroCounter longBreakTimer
    _ ->
      C.hCenter (label "Pomodoro" <+> padLeftRight 2 (label "Short break") <+> label "Long break") <=> timerAndPomodoroCounter pomodoroTimer
  where timerAndPomodoroCounter timerLens = drawTimer (s ^. timerRunning) (s ^. timerLens)
          <=> drawPomodorosCounter s

label :: String -> Widget Name
label s = B.border $ padLeftRight 1 $ str s

drawTimer :: Bool -> Int -> Widget Name
drawTimer active timerDuration =
  C.hCenter $
    padTopBottom 1 $
    if active then
      withAttr timerAttr timerWidget
    else timerWidget
  where timerWidget = padTopBottom 1 $ padLeftRight 1
          $ str $ formatTimer timerDuration
        

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
