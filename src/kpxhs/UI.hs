module UI (drawUI) where

import Brick.Types  (Widget)
import Lens.Micro   ((^.))

import Types
    ( Field
    , State
    , View (EntryView, ExitView, PasswordView)
    , activeView
    )
import UI.BrowserUI (drawBrowser)
import UI.DialogUI  (drawDialog)
import UI.EntryUI   (drawEntryDetails)
import UI.ExitUI    (drawExitView)


drawUI :: State -> [Widget Field]
drawUI st = case st ^. activeView of
  PasswordView -> drawDialog st
  EntryView    -> drawEntryDetails st
  ExitView     -> drawExitView st
  _            -> drawBrowser st
