module UI.Config (drawConfigList, timerDialog)
where
import Resources (Name, ConfigSetting, configLabel, configValue, configSettingsValueToText, Timer (..), TimerDialogChoice (Increase, Decrease))
import Brick (Widget, withAttr, txt, hBox, vLimit, padLeftRight)
import qualified Brick.Widgets.List as BL
import UI.Attributes (selectedConfigAttr)
import Control.Lens ((^.))
import Brick.Widgets.Core (fill)
import Brick.Widgets.Dialog (dialog, Dialog)

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

timerDialog :: Timer -> Dialog TimerDialogChoice
timerDialog timer = 
    dialog title (Just (0, options)) 70
        where
            options = [("Increase by 1min", Increase), ("Decrease by 1min", Decrease)]
            title   = case timer of
                Pomodoro -> Just "Pomodoro initial timer"
                ShortBreak -> Just "Short break initial timer"
                LongBreak -> Just "Long break initial timer"