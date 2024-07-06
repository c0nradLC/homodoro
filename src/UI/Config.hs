module UI.Config (
    drawConfig,
)
where

import Brick (BrickEvent (AppEvent), EventM, Widget, str)
import Control.Monad.State.Class (get)
import Resources (AppState, Name, Tick (Tick))
import Timer (tickTimer)

drawConfig :: Widget Name
drawConfig = str "Config screen"

handleConfigEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleConfigEvent ev =
    case ev of
        (AppEvent Tick) -> do
            s <- get
            tickTimer s
