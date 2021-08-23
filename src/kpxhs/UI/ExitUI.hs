module UI.ExitUI (drawExitView) where

import Lens.Micro ( (^.) )
import Brick.Types (Widget)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (str, padAll)

import Types ( exitDialog, Field, State )


drawExitView :: State -> [Widget Field]
drawExitView st = [ui]
  where
    ui = D.renderDialog (st^.exitDialog)
         $ C.hCenter
         $ padAll 1
         $ str "Do you want to clear the clipboard before exiting?"
