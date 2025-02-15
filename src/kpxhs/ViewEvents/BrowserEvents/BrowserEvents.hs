{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.BrowserEvents.BrowserEvents (browserEvent) where

import qualified Brick.Main         as M
import qualified Brick.Types        as T
import           Brick.Widgets.Core (str)
import           Brick.Widgets.List (listMoveToBeginning, listMoveToEnd)
import qualified Brick.Widgets.List as L
import           Data.Char          (isDigit)
import           Data.Maybe         (fromMaybe)
import qualified Graphics.Vty       as V
import           Lens.Micro         ((%~), (&), (.~), (^.))

import Types
    ( CopyType (CopyPassword, CopyUsername)
    , Event
    , Field
    , State
    , currentCmd
    , currentPath
    , footer
    , isClipboardCleared
    , visibleEntries
    )
import ViewEvents.BrowserEvents.Core       (goUpParent)
import ViewEvents.BrowserEvents.Event      (handleAppEvent)
import ViewEvents.BrowserEvents.Fork       (handleEnter)
import ViewEvents.BrowserEvents.Utils      (listMovePageDown, listMovePageUp)
import ViewEvents.BrowserEvents.VimCommand (handleVimDigit, handleVimMotion)
import ViewEvents.Common
    ( commonTabEvent
    , liftContinue
    , prepareExit
    , updateFooterGuarded
    )
import ViewEvents.Copy                     (copyEntryCommon)
import ViewEvents.Utils                    (getSelectedEntry, isCopyable)

browserEvent :: State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State)
browserEvent =
  commonTabEvent
    ( \st e ->
        case e of
          T.VtyEvent (V.EvKey V.KEsc [])        -> handleEsc st
          T.VtyEvent (V.EvKey (V.KChar 'q') []) -> handleQuit st
          T.VtyEvent (V.EvKey V.KEnter [])      -> handleEnter st
          T.VtyEvent (V.EvKey (V.KChar 'p') []) | isCopyable st -> copyPassword st
          T.VtyEvent (V.EvKey (V.KChar 'u') []) | isCopyable st -> copyUsername st
          T.VtyEvent ev                         -> M.continue $ handleNav ev st
          T.AppEvent ev                         -> liftContinue $ handleAppEvent st ev
          _                                     -> M.continue st
    )

handleEsc :: State -> T.EventM Field (T.Next State)
handleEsc st =
  M.continue $ case st^.currentCmd of
    "" -> st
    _  -> st & currentCmd .~ ""

handleQuit :: State -> T.EventM Field (T.Next State)
handleQuit st =
  case (st^.currentPath, st^.isClipboardCleared) of
    ([], False) -> M.continue $ prepareExit st
    ([], True)  -> M.halt st
    _           -> M.continue $ goUpParent st

copyPassword :: State -> T.EventM Field (T.Next State)
copyPassword st = liftContinue $ copyEntryFromBrowser st CopyPassword

copyUsername :: State -> T.EventM Field (T.Next State)
copyUsername st = liftContinue $ copyEntryFromBrowser st CopyUsername

copyEntryFromBrowser :: State -> CopyType -> IO State
copyEntryFromBrowser st ctype =
  fromMaybe def (getSelectedEntry f st)
    where
      def = pure $ st & footer .~ str "No entry selected!"
      f entry = copyEntryCommon st entry ctype

handleNav :: V.Event -> State -> State
handleNav e st = new_st & updateFooterGuarded
  where
    noCmd = null $ st ^. currentCmd
    f x = st & visibleEntries %~ x
    g x = f x & currentCmd .~ ""
    new_st =
      case e of
        -- Keys from handleListEvent (not affected by vim commands)
        V.EvKey V.KUp []                     -> g L.listMoveUp
        V.EvKey V.KDown []                   -> g L.listMoveDown
        V.EvKey V.KHome []                   -> g listMoveToBeginning
        V.EvKey V.KEnd []                    -> g listMoveToEnd
        V.EvKey V.KPageDown []               -> g listMovePageDown
        V.EvKey V.KPageUp []                 -> g listMovePageUp
        -- Additional keys that are not affected by vim commands
        V.EvKey (V.KChar 'g') []             -> g listMoveToBeginning
        V.EvKey (V.KChar 'G') []             -> g listMoveToEnd
        -- Keys that take an optional count before them
        V.EvKey (V.KChar 'w') [] | noCmd     -> f L.listMoveUp
        V.EvKey (V.KChar 's') [] | noCmd     -> f L.listMoveDown
        V.EvKey (V.KChar 'k') [] | noCmd     -> f L.listMoveUp
        V.EvKey (V.KChar 'j') [] | noCmd     -> f L.listMoveDown
        -- Vim commands
        V.EvKey (V.KChar x) []   | isDigit x -> handleVimDigit st x
        V.EvKey (V.KChar x) []               -> handleVimMotion st x
        _                                    -> st
