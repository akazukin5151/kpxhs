module UI.Common (getEditor) where

import qualified Brick.Focus        as F
import           Brick.Types        (Widget)
import           Brick.Widgets.Core (txt)
import qualified Brick.Widgets.Edit as E
import           Data.Text          (Text)
import           Lens.Micro         (Getting, (^.))

import Types (Field, State, focusRing)


getEditor :: State
          -> Getting (E.Editor Text Field) State (E.Editor Text Field)
          -> ([Text] -> Text)
          -> Widget Field
getEditor st f g =
  F.withFocusRing (st ^. focusRing) (E.renderEditor (txt . g)) (st ^. f)
