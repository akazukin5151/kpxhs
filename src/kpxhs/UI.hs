module UI (drawUI) where

import Brick.Types (Widget)
import Lens.Micro  ((^.))

import Types
    ( Field
    , State
    , View (EntryDetailsView, ExitView, LoginView)
    , activeView
    )
import UI.BrowserUI (drawBrowser)
import UI.LoginUI  (drawDialog)
import UI.EntryUI   (drawEntryDetails)
import UI.ExitUI    (drawExitView)


drawUI :: State -> [Widget Field]
drawUI st = case st ^. activeView of
  LoginView        -> drawDialog       st
  EntryDetailsView -> drawEntryDetails st
  ExitView         -> drawExitView     st
  _                -> drawBrowser      st
