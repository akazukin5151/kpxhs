module UI (drawUI) where

import Lens.Micro
import Brick.Types (Widget)

import Types
import UI.DialogUI
import UI.EntryUI
import UI.ExitUI
import UI.BrowserUI


drawUI :: State -> [Widget Field]
drawUI st = case st ^. activeView of
  PasswordView -> drawDialog st
  EntryView -> drawEntryDetails st
  ExitView -> drawExitView st
  _ -> drawBrowser st

