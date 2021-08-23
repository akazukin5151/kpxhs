module UI.Common (getEditor) where

import qualified Brick.Focus        as F
import           Brick.Types        (Widget)
import           Brick.Widgets.Core (str)
import qualified Brick.Widgets.Edit as E
import           Lens.Micro         (Getting, (^.))

import           Types              (Field, State, focusRing)


getEditor :: State
          -> Getting (E.Editor String Field) State (E.Editor String Field)
          -> ([String] -> String)
          -> Widget Field
getEditor st f g =
  F.withFocusRing (st ^. focusRing) (E.renderEditor (str . g)) (st ^. f)
