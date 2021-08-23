module Events (appEvent) where

import Brick.Types               (BrickEvent, EventM, Next)
import Lens.Micro                ((^.))

import Types
    ( Field
    , State
    , View (BrowserView, EntryView, ExitView, PasswordView, SearchView)
    , activeView
    )
import ViewEvents.BrowserEvents  (browserEvent)
import ViewEvents.EntryEvents    (entryDetailsEvent)
import ViewEvents.ExitEvents     (exitEvent)
import ViewEvents.PasswordEvents (passwordEvent)
import ViewEvents.SearchEvents   (searchEvent)


appEvent :: State -> BrickEvent Field e -> EventM Field (Next State)
appEvent st e =
  case st ^. activeView of
    PasswordView -> passwordEvent st e
    EntryView    -> entryDetailsEvent st e
    SearchView   -> searchEvent st e
    BrowserView  -> browserEvent st e
    ExitView     -> exitEvent st e
