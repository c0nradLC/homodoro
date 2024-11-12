{-# LANGUAGE OverloadedStrings #-}

module UI.Config (drawConfigList, timerDialog) where

import Brick (Widget, hBox, padLeftRight, txt, vLimit, withAttr)
import Brick.Widgets.Core (fill)
import Brick.Widgets.Dialog (Dialog, dialog)
import qualified Brick.Widgets.List as BL
import Config (configSettingsValueToText)
import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import Resources (ConfigSetting, Name, Timer (..), TimerDialogChoice (..), configLabel, configValue)
import UI.Attributes (selectedConfigAttr)

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
            [ txt (cfg ^. configLabel),
              fill ' ',
              txt $ configSettingsValueToText $ cfg ^. configValue
            ]

timerDialog :: Maybe Int -> Timer -> Dialog TimerDialogChoice
timerDialog selectedButtonIndex timer =
  let btnIdx = fromMaybe 0 selectedButtonIndex
   in dialog title (Just (btnIdx, options)) 50
  where
    options = [("Ok", Ok), ("Reset active timer", ResetTimer)]
    title = case timer of
      Pomodoro -> Just "Set pomodoro initial timer"
      ShortBreak -> Just "Set short break initial timer"
      LongBreak -> Just "Set long break initial timer"
