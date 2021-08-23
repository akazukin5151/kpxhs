module Events (appEvent) where

import Lens.Micro ( (^.) )
import qualified Brick.Types as T

import Types
    ( activeView,
      Field,
      State,
      View(ExitView, PasswordView, EntryView, SearchView, BrowserView) )
import ViewEvents.BrowserEvents ( browserEvent )
import ViewEvents.EntryEvents ( entryDetailsEvent )
import ViewEvents.PasswordEvents ( passwordEvent )
import ViewEvents.SearchEvents ( searchEvent )
import ViewEvents.ExitEvents ( exitEvent )


appEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
appEvent st e =
  case st ^. activeView of
    PasswordView -> passwordEvent st e
    EntryView -> entryDetailsEvent st e
    SearchView -> searchEvent st e
    BrowserView -> browserEvent st e
    ExitView -> exitEvent st e
