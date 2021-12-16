module Events (appEvent) where

import Brick.Types (BrickEvent, EventM, Next)
import Lens.Micro  ((^.))

import Types
    ( Event
    , Field
    , State
    , View (BrowserView, EntryDetailsView, ExitDialogView, LoginView, SearchView, LoginFrozenView)
    , activeView
    )
import ViewEvents.BrowserEvents.BrowserEvents (browserEvent)
import ViewEvents.EntryDetailsEvents          (entryDetailsEvent)
import ViewEvents.ExitDialogEvents            (exitEvent)
import ViewEvents.LoginEvents                 (passwordEvent)
import ViewEvents.LoginFrozenEvents           (loginFrozenEvent)
import ViewEvents.SearchEvents                (searchEvent)


appEvent :: State -> BrickEvent Field Event -> EventM Field (Next State)
appEvent st e = f st e
  where
    f = case st ^. activeView of
          LoginView        -> passwordEvent
          LoginFrozenView  -> loginFrozenEvent
          EntryDetailsView -> entryDetailsEvent
          SearchView       -> searchEvent
          BrowserView      -> browserEvent
          ExitDialogView   -> exitEvent
