{-# LANGUAGE OverloadedStrings #-}

module UI.Config (drawConfigList) where

import Brick (Widget, hBox, padLeftRight, txt, vLimit, withAttr)
import Brick.Widgets.Core (fill)
import qualified Brick.Widgets.List as BL
import Config (configSettingsValueToText)
import Control.Lens ((^.))
import Resources (ConfigSetting, Name, configLabel, configValue)
import UI.Attributes (selectedConfigAttr)

drawConfigList :: BL.List Name ConfigSetting -> Widget Name
drawConfigList cfg = do
    BL.renderList drawConfig True (BL.listRemove 3 cfg)

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
