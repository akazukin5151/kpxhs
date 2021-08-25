module Events (appEvent) where

import Brick.Types (BrickEvent, EventM, Next)
import Lens.Micro  ((^.))

import Types
    ( Event
    , Field
    , State
    , View (BrowserView, EntryView, ExitView, PasswordView, SearchView)
    , activeView
    )
import ViewEvents.BrowserEvents  (browserEvent)
import ViewEvents.EntryEvents    (entryDetailsEvent)
import ViewEvents.ExitEvents     (exitEvent)
import ViewEvents.LoginEvents (passwordEvent)
import ViewEvents.SearchEvents   (searchEvent)


appEvent :: State -> BrickEvent Field Event -> EventM Field (Next State)
appEvent st e = f st e
  where
    f = case st ^. activeView of
          PasswordView -> passwordEvent
          EntryView    -> entryDetailsEvent
          SearchView   -> searchEvent
          BrowserView  -> browserEvent
          ExitView     -> exitEvent
