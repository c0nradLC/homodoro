{-# LANGUAGE OverloadedStrings #-}

module UI.Config (drawConfigList, timerDialog)
where
import Resources (Name, ConfigSetting, configLabel, configValue, Timer (..), TimerDialogChoice (..))
import Brick (Widget, withAttr, txt, hBox, vLimit, padLeftRight)
import qualified Brick.Widgets.List as BL
import UI.Attributes (selectedConfigAttr)
import Control.Lens ((^.))
import Brick.Widgets.Core (fill)
import Brick.Widgets.Dialog (dialog, Dialog)
import Config (configSettingsValueToText)
import Data.Maybe (fromMaybe)

drawConfigList :: BL.List Name ConfigSetting -> Widget Name
drawConfigList cfg = do
    BL.renderList drawConfig True cfg

drawConfig :: Bool -> ConfigSetting -> Widget Name
drawConfig selected cfg = do
    if selected then
        withAttr selectedConfigAttr configWidget
    else
        configWidget
    where configWidget = vLimit 1 $ padLeftRight 1 $ hBox
                [ txt (cfg ^. configLabel)
                , fill ' '
                , txt $ configSettingsValueToText $ cfg ^. configValue ]

timerDialog :: Maybe Int -> Timer -> Dialog TimerDialogChoice
timerDialog selectedButtonIndex timer =
    let btnIdx = fromMaybe 0 selectedButtonIndex
    in
    dialog title (Just (btnIdx, options)) 50
        where
            options = [("Ok", Ok), ("Reset active timer", ResetTimer)]
            title   = case timer of
                Pomodoro -> Just "Set pomodoro initial timer"
                ShortBreak -> Just "Set short break initial timer"
                LongBreak -> Just "Set long break initial timer"