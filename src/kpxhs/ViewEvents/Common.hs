-- Functions for further composition and combining
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.Common where

import qualified Brick.Focus            as F
import qualified Brick.Main             as M
import           Brick.Types            (Widget)
import qualified Brick.Types            as T
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (isJust)
import           Data.Text              (Text)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((%~), (&), (.~), (^.))

import Common           (annotate, defaultDialog, exit, initialFooter, tab)
import Types
    ( Event (ClearClipCount)
    , Field
    , State
    , View (BrowserView, EntryDetailsView, ExitView, PasswordView, SearchView)
    , activeView
    , countdownThreadId
    , currentPath
    , exitDialog
    , focusRing
    , footer
    , previousView
    )
import ViewEvents.Copy  (handleClipCount)
import ViewEvents.Utils (isCopyable)


liftContinue1 :: (a -> IO b) -> a -> T.EventM n (T.Next b)
liftContinue1 g st = liftIO (g st) >>= M.continue

liftContinue2 :: (a -> b -> IO c) -> a -> b -> T.EventM n (T.Next c)
liftContinue2 g st x = liftIO (g st x) >>= M.continue

prepareExit :: State -> State
prepareExit st =
  st & previousView .~ (st^.activeView)
     & exitDialog   .~ defaultDialog
     & activeView   .~ ExitView

commonTabEvent :: (State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State))
               -> State
               -> T.BrickEvent Field Event
               -> T.EventM Field (T.Next State)
commonTabEvent fallback st e =
  case e of
    T.VtyEvent (V.EvKey (V.KChar '\t') []) -> f focusNext
    T.VtyEvent (V.EvKey V.KBackTab [])     -> f focusPrev
    T.AppEvent (ClearClipCount count)      -> liftContinue2 handleClipCount st count
    _                                      -> fallback st e
  where
    f x = M.continue $ _handleTab st x (st ^. activeView)

_handleTab :: State -> (State -> View -> State) -> View -> State
_handleTab st f BrowserView = f st SearchView
_handleTab st f _           = f st BrowserView

focus :: (F.FocusRing Field -> F.FocusRing Field) -> State -> View -> State
focus f st view =
  st & focusRing  %~ f
     & activeView .~ view
     & updateFooterGuarded

focusNext :: State -> View -> State
focusNext = focus F.focusNext

focusPrev :: State -> View -> State
focusPrev = focus F.focusPrev

-- | Restores the default footer for the current view
--  Should be only used when transitioning to a new view or field
updateFooter :: State -> State
updateFooter st = st & footer .~ viewDefaultFooter st

updateFooterGuarded :: State -> State
updateFooterGuarded st =
  if isJust $ st ^. countdownThreadId
     then st
     else st & updateFooter

viewDefaultFooter :: State -> Widget Field
viewDefaultFooter st =
  annotate $ case st^.activeView of
    SearchView       -> [exit, tab " focus list "]
    EntryDetailsView -> [back, username, password]
    PasswordView     -> initialFooter $ st ^. focusRing
    ExitView         -> [("", "")]
    BrowserView      ->
      let extra = if isCopyable st then [username, password] else [] in
      case st^.currentPath of
        [] -> [exit, focus_search] <> extra
        _  -> [back, focus_search] <> extra
  where
    back = ("Esc", " back  ")
    username = ("u", " copy username  ")
    password = ("p", " copy password")

focus_search :: (Text, Text)
focus_search = ("Tab", " focus search  ")
