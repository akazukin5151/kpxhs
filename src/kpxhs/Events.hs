module Events (appEvent) where

import Brick.Types               (BrickEvent, EventM, Next)
import Lens.Micro                ((^.))

import ViewEvents.BrowserEvents  (browserEvent)
import ViewEvents.EntryEvents    (entryDetailsEvent)
import ViewEvents.ExitEvents     (exitEvent)
import ViewEvents.PasswordEvents (passwordEvent)
import ViewEvents.SearchEvents   (searchEvent)
import Types
    ( Field
    , State
    , View (BrowserView, EntryView, ExitView, PasswordView, SearchView)
    , activeView, Event
    )


appEvent :: State -> BrickEvent Field Event -> EventM Field (Next State)
appEvent st e = f st e
  where
    f = case st ^. activeView of
          PasswordView -> passwordEvent
          EntryView    -> entryDetailsEvent
          SearchView   -> searchEvent
          BrowserView  -> browserEvent
          ExitView     -> exitEvent
