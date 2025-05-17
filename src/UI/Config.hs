{-# LANGUAGE OverloadedStrings #-}

module UI.Config (drawConfigList, initialTimerDialog, drawInitialTimerDialog, soundVolumeDialog, drawSoundVolumeDialog) where

import Brick (Padding (..), Widget, hBox, padBottom, padLeftRight, padTop, str, txt, vBox, vLimit, withAttr, (<=>))
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (fill)
import Brick.Widgets.Dialog (Dialog, dialog)
import qualified Brick.Widgets.List as BL
import Config (configSettingsValueToString)
import Control.Lens ((^.))
import Types (ConfigSetting, InitialTimerDialogChoice (CloseInitialTimerDialog), Name, SoundVolumeDialogChoice (CloseSoundVolumeDialog, PlayTestAudio), Timer (..), configLabel, configValue)
import UI.Attributes (blackOnWhiteAttr, selectedConfigAttr, timerAttr)
import UI.Timer (formatTimer)

drawConfigList :: BL.List Name ConfigSetting -> Widget Name
drawConfigList cfg = do
    BL.renderList drawConfig True cfg

drawConfig :: Bool -> ConfigSetting -> Widget Name
drawConfig selected cfg = do
    if selected
        then withAttr selectedConfigAttr configWidget
        else configWidget
  where
    configWidget =
        vLimit 1 $
            padLeftRight 1 $
                hBox
                    [ str (cfg ^. configLabel)
                    , fill ' '
                    , str $ configSettingsValueToString $ cfg ^. configValue
                    ]

initialTimerDialog :: Maybe Int -> Timer -> Dialog InitialTimerDialogChoice
initialTimerDialog _ timer = dialog title (Just (0, options)) 50
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
        , C.hCenter $ padTop (Pad 1) $ txt "[Up arrow] - Increase by 1min"
        , C.hCenter (padBottom (Pad 1) $ txt "[Down arrow] - Decrease by 1min")
        ]

soundVolumeDialog :: Maybe String -> Maybe Int -> Dialog SoundVolumeDialogChoice
soundVolumeDialog title _ = dialog title (Just (1, options)) 50
  where
    options = [("Close", CloseSoundVolumeDialog), ("Play test audio", PlayTestAudio)]

drawSoundVolumeDialog :: String -> Int -> Widget Name
drawSoundVolumeDialog title vol =
    vBox
        [ padTop (Pad 1) $
            C.hCenter (str title)
                <=> C.hCenter (withAttr blackOnWhiteAttr (padLeftRight 1 $ str $ show vol))
        , C.hCenter $ padTop (Pad 1) $ txt "[Up arrow] - Increase by 5"
        , C.hCenter $ padBottom (Pad 1) $ txt "[Down arrow] - Decrease by 5"
        ]
