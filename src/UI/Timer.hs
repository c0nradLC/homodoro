module UI.Timer
(
    drawTimers
)
where
import Resources (AppState, Name (TaskList), Timer (Pomodoro, ShortBreak, LongBreak), focus, pomodoroTimer, shortBreakTimer, longBreakTimer, pomodoroCycleCounter)
import Brick (Widget, withAttr, padLeftRight, str, (<+>), (<=>), padTopBottom)
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Center as C
import Control.Lens ( (^.) )
import UI.Attributes (selectedTimerAttr, timerAttr)
import qualified Brick.Widgets.Border as B
import Text.Printf (printf)

drawTimers :: AppState -> Widget Name
drawTimers s =
    case BF.focusGetCurrent (s ^. focus) of
        Just (TaskList timer) -> case timer of
            Pomodoro ->
                (C.hCenter (withAttr selectedTimerAttr (label "Pomodoro") <+> padLeftRight 2 (label "Short break") <+> label "Long break") <=>) $
                    drawTimer (s ^. pomodoroTimer)
                        <=> drawCyclesCounter s
            ShortBreak ->
                (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (withAttr selectedTimerAttr $ label "Short break") <+> label "Long break") <=>) $
                    drawTimer (s ^. shortBreakTimer)
                        <=> drawCyclesCounter s
            LongBreak ->
                (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (label "Short break") <+> withAttr selectedTimerAttr (label "Long break")) <=>) $
                    drawTimer (s ^. longBreakTimer)
                        <=> drawCyclesCounter s
        _ ->
            (C.hCenter (label "Pomodoro" <+> padLeftRight 2 (label "Short break") <+> label "Long break") <=>) $
                drawTimer (s ^. pomodoroTimer)
                    <=> drawCyclesCounter s

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

drawCyclesCounter :: AppState -> Widget Name
drawCyclesCounter s = C.hCenter (label (formatCycleCounter (s ^. pomodoroCycleCounter)))

formatTimer :: Int -> String
formatTimer timer =
    let minutes = timer `div` 60
        seconds = timer `mod` 60
     in printf "%02d:%02d" minutes seconds

formatCycleCounter :: Int -> String
formatCycleCounter =
    printf "Pomodoro cycles: %01d"