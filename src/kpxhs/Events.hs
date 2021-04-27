module Events (appEvent) where

import Lens.Micro
import qualified Brick.Types as T

import Types
import ViewEvents.BrowserEvents
import ViewEvents.EntryEvents
import ViewEvents.PasswordEvents
import ViewEvents.SearchEvents


appEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
appEvent st e =
  case st ^. activeView of
    PasswordView -> passwordEvent st e
    EntryView -> entryDetailsEvent st e
    SearchView -> searchEvent st e
    BrowserView -> browserEvent st e
