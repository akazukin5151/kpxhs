module UI (drawUI) where

import Brick.Types (Widget)
import Lens.Micro  ((^.))

import Types
    ( Field
    , State
    , View (EntryDetailsView, ExitDialogView, LoginView)
    , activeView
    )
import UI.BrowserUI      (drawBrowser)
import UI.EntryDetailsUI (drawEntryDetails)
import UI.ExitUI         (drawExitDialogView)
import UI.LoginUI        (drawDialog)


drawUI :: State -> [Widget Field]
drawUI st = case st ^. activeView of
  LoginView        -> drawDialog       st
  EntryDetailsView -> drawEntryDetails st
  ExitDialogView   -> drawExitDialogView     st
  _                -> drawBrowser      st
