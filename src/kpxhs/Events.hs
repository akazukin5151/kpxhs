module Events (appEvent) where

import Brick.Types (BrickEvent, EventM, Next)
import Lens.Micro  ((^.))

import Types
    ( Event
    , Field
    , State
    , View (BrowserView, EntryDetailsView, ExitDialogView, LoginView, SearchView)
    , activeView
    )
import ViewEvents.BrowserEvents.BrowserEvents (browserEvent)
import ViewEvents.EntryDetailsEvents          (entryDetailsEvent)
import ViewEvents.ExitEvents                  (exitEvent)
import ViewEvents.LoginEvents                 (passwordEvent)
import ViewEvents.SearchEvents                (searchEvent)


appEvent :: State -> BrickEvent Field Event -> EventM Field (Next State)
appEvent st e = f st e
  where
    f = case st ^. activeView of
          LoginView        -> passwordEvent
          EntryDetailsView -> entryDetailsEvent
          SearchView       -> searchEvent
          BrowserView      -> browserEvent
          ExitDialogView   -> exitEvent
