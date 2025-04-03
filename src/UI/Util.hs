module UI.Util (changeFocus) where

import Brick (EventM)
import qualified Brick.Focus as BF
import Control.Lens
import Resources (AppState, Name, focus)

changeFocus :: Name -> AppState -> EventM Name AppState ()
changeFocus nextFocus s = focus .= BF.focusSetCurrent nextFocus (s ^. focus)
