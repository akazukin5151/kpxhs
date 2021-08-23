module UI (drawUI) where

import Lens.Micro ( (^.) )
import Brick.Types (Widget)

import Types
    ( activeView,
      Field,
      State,
      View(ExitView, PasswordView, EntryView) )
import UI.DialogUI ( drawDialog )
import UI.EntryUI ( drawEntryDetails )
import UI.ExitUI ( drawExitView )
import UI.BrowserUI ( drawBrowser )


drawUI :: State -> [Widget Field]
drawUI st = case st ^. activeView of
  PasswordView -> drawDialog st
  EntryView -> drawEntryDetails st
  ExitView -> drawExitView st
  _ -> drawBrowser st
