module Util (changeFocus) where
import Resources (Name, AppState, focus)
import Brick (EventM)
import qualified Brick.Focus as BF
import Control.Lens

changeFocus :: Name -> AppState -> EventM Name AppState ()
changeFocus nextFocus s = focus .= BF.focusSetCurrent nextFocus (s ^. focus)