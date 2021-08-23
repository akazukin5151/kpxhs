module UI.Common (getEditor) where

import Lens.Micro ( Getting, (^.) )
import qualified Brick.Widgets.Edit as E
import Brick.Types (Widget)
import qualified Brick.Focus as F
import Brick.Widgets.Core (str)

import Types ( focusRing, Field, State )


getEditor :: State
          -> Getting (E.Editor String Field) State (E.Editor String Field)
          -> ([String] -> String)
          -> Widget Field
getEditor st f g =
  F.withFocusRing (st ^. focusRing) (E.renderEditor (str . g)) (st ^. f)
