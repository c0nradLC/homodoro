{-# LANGUAGE OverloadedStrings #-}

module UI.Timer (
    drawTimers,
    formatTimer,
    timerDialog,
    drawInitialTimerDialog,
)
where

import Brick (Padding (Pad), Widget, padBottom, padLeftRight, padTop, padTopBottom, str, txt, vBox, withAttr, (<+>), (<=>))
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import Resources (AppState, Name (TaskList), Timer (LongBreak, Pomodoro, ShortBreak), focus, longBreakTimer, pomodoroCounter, pomodoroTimer, shortBreakTimer, timerRunning, InitialTimerDialogChoice (CloseInitialTimerDialog))
import Text.Printf (printf)
import UI.Attributes (selectedTimerAttr, timerAttr)
import Brick.Widgets.Dialog (Dialog, dialog)
import Data.Maybe (fromMaybe)

drawTimers :: AppState -> Widget Name
drawTimers s =
    case BF.focusGetCurrent (s ^. focus) of
        Just (TaskList timer) -> case timer of
            Pomodoro ->
                B.border $
                    C.hCenter
                        ( withAttr selectedTimerAttr (label "Pomodoro")
                            <+> padLeftRight 2 (label "Short break")
                            <+> label "Long break"
                        )
                        <=> timerAndPomodoroCounter pomodoroTimer
            ShortBreak ->
                B.border $
                    C.hCenter
                        ( label "Pomodoro"
                            <+> padLeftRight 2 (withAttr selectedTimerAttr $ label "Short break")
                            <+> label "Long break"
                        )
                        <=> timerAndPomodoroCounter shortBreakTimer
            LongBreak ->
                B.border $
                    C.hCenter
                        ( label "Pomodoro"
                            <+> padLeftRight 2 (label "Short break")
                            <+> withAttr selectedTimerAttr (label "Long break")
                        )
                        <=> timerAndPomodoroCounter longBreakTimer
        _ ->
            B.border $ C.hCenter (label "Pomodoro" <+> padLeftRight 2 (label "Short break") <+> label "Long break") <=> timerAndPomodoroCounter pomodoroTimer
  where
    timerAndPomodoroCounter timerLens =
        drawTimer (s ^. timerRunning) (s ^. timerLens)
            <=> drawPomodorosCounter s

label :: String -> Widget Name
label s = B.border $ padLeftRight 1 $ str s

drawTimer :: Bool -> Int -> Widget Name
drawTimer active timerDuration =
    C.hCenter $
        padTopBottom 1 $
            if active
                then withAttr timerAttr timerWidget
                else timerWidget
  where
    timerWidget =
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

timerDialog :: Maybe Int -> Timer -> Dialog InitialTimerDialogChoice
timerDialog selectedButtonIndex timer =
    let btnIdx = fromMaybe 0 selectedButtonIndex
     in dialog title (Just (btnIdx, options)) 50
  where
    options = [("Close", CloseInitialTimerDialog)]
    title = case timer of
        Pomodoro -> Just "Set pomodoro initial timer"
        ShortBreak -> Just "Set short break initial timer"
        LongBreak -> Just "Set long break initial timer"

drawInitialTimerDialog :: Int -> Widget Name
drawInitialTimerDialog currentInitialTimer =
    vBox
        [ padTop (Pad 1) $
            C.hCenter (txt "Initial timer")
                <=> C.hCenter (withAttr timerAttr (padLeftRight 1 $ str $ formatTimer currentInitialTimer))
        , C.hCenter $ padTop (Pad 1) $ txt "[Up arrow]   - Increase by 1min"
        , C.hCenter $ padBottom (Pad 1) $ txt "[Down arrow] - Decrease by 1min"
        ]
