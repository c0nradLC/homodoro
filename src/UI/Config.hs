module UI.Config 
(
    drawConfig
)
where
import Control.Monad.State.Class (get)
import Brick (Widget, str, BrickEvent (AppEvent), EventM)
import Resources (Name, Tick (Tick), AppState)
import Timer (tickTimer)

drawConfig :: Widget Name
drawConfig = str "Config screen"

handleConfigEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleConfigEvent ev =
    case ev of
        (AppEvent Tick) -> do
            s <- get
            tickTimer s