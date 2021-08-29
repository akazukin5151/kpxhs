module UI.ExitDialogUI (drawExitDialogView) where

import           Brick.Types          (Widget)
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (padAll, str)
import qualified Brick.Widgets.Dialog as D
import           Lens.Micro           ((^.))
import           Types                (Field, State, exitDialog)


drawExitDialogView :: State -> [Widget Field]
drawExitDialogView st = [ui]
  where
    ui = D.renderDialog (st^.exitDialog)
         $ C.hCenter
         $ padAll 1
         $ str "Do you want to clear the clipboard before exiting?"
