{-# LANGUAGE OverloadedStrings #-}

module UI.Config (drawConfigList, initialTimerDialog, drawInitialTimerDialog) where

import Brick (Widget, hBox, padLeftRight, txt, vLimit, withAttr, vBox, padTop, Padding (..), (<=>), padBottom, str)
import Brick.Widgets.Core (fill)
import qualified Brick.Widgets.List as BL
import Config (configSettingsValueToText)
import Control.Lens ((^.))
import Resources (ConfigSetting, Name, configLabel, configValue, Timer (..), InitialTimerDialogChoice (CloseInitialTimerDialog))
import UI.Attributes (selectedConfigAttr, timerAttr)
import Brick.Widgets.Dialog (Dialog, dialog)
import qualified Brick.Widgets.Center as C
import UI.Timer (formatTimer)
import Data.Maybe

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
                    [ txt (cfg ^. configLabel)
                    , fill ' '
                    , txt $ configSettingsValueToText $ cfg ^. configValue
                    ]

initialTimerDialog :: Maybe Int -> Timer -> Dialog InitialTimerDialogChoice
initialTimerDialog selectedButtonIndex timer =
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