-- Functions for further composition and combining
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.Common where

import qualified Brick.Main             as M
import           Brick.Types            (Widget)
import qualified Brick.Types            as T
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (isJust)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((&), (.~), (^.))

import Common           (annotate, defaultDialog, initialFooter)
import Types
    ( Event (ClearClipCount)
    , Field
    , State
    , View (BrowserView, EntryDetailsView, ExitDialogView, LoginView, SearchView, LoginFrozenView)
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
     & activeView   .~ ExitDialogView

commonTabEvent :: (State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State))
               -> State
               -> T.BrickEvent Field Event
               -> T.EventM Field (T.Next State)
commonTabEvent fallback st e =
  case e of
    T.VtyEvent (V.EvKey (V.KChar '/') []) -> M.continue $ focusSearch st
    T.AppEvent (ClearClipCount count)     -> liftContinue2 handleClipCount st count
    _                                     -> fallback st e

focusSearch :: State -> State
focusSearch st =
  st & activeView .~ SearchView
     & updateFooterGuarded

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
    SearchView       -> [esc " focus list "]
    EntryDetailsView -> [back, username, password]
    LoginView        -> initialFooter $ st ^. focusRing
    LoginFrozenView  -> initialFooter $ st ^. focusRing
    ExitDialogView   -> [("", "")]
    BrowserView      ->
      let extra = if isCopyable st then [view_details, username, password] else [open_folder] in
      case st^.currentPath of
        [] -> [exitq, focus_search] <> extra
        _  -> [backq, focus_search] <> extra
  where
    esc x = ("Esc", x)
    exitq = ("q", " exit  ")
    back = esc " back  "
    backq = ("q", " back  ")
    username = ("u", " copy username  ")
    password = ("p", " copy password")
    focus_search = ("/", " search  ")
    view_details = ("Enter", " details  ")
    open_folder = ("Enter", " open folder  ")
