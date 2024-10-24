module UI.Config (drawConfigList)
where
import Resources (Name, ConfigSetting, configLabel, configValue, configSettingsValueToText)
import Brick (Widget, withAttr, txt, hBox, vLimit, padLeftRight)
import qualified Brick.Widgets.List as BL
import UI.Attributes (selectedConfigAttr)
import Control.Lens ((^.))
import Brick.Widgets.Core (fill)

drawConfigList :: BL.List Name ConfigSetting -> Widget Name
drawConfigList cfg = do
    BL.renderList drawConfig True cfg

drawConfig :: Bool -> ConfigSetting -> Widget Name
drawConfig sel cfg = do
    if sel then
        vLimit 1 $ withAttr selectedConfigAttr $ padLeftRight 1 $ hBox
        [ txt (cfg ^. configLabel)
        , fill ' '
        , txt $ configSettingsValueToText $ cfg ^. configValue ]
    else 
        vLimit 1 $ padLeftRight 1 $ hBox 
        [ txt (cfg ^. configLabel)
        , fill ' '
        , txt $ configSettingsValueToText $ cfg ^. configValue ]
