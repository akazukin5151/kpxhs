{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.BrowserEvents.BrowserEvents (browserEvent) where

import qualified Brick.Main         as M
import qualified Brick.Types        as T
import           Brick.Util         (clamp)
import           Brick.Widgets.Core (str)
import qualified Brick.Widgets.List as L
import           Data.Maybe         (fromMaybe)
import qualified Graphics.Vty       as V
import           Lens.Micro         ((%~), (&), (.~), (^.))

import Types
    ( CopyType (CopyPassword, CopyUsername)
    , Event
    , Field
    , State
    , currentPath
    , footer
    , hasCopied
    , visibleEntries
    )
import ViewEvents.BrowserEvents.Event (handleAppEvent)
import ViewEvents.BrowserEvents.Fork  (handleEnter)
import ViewEvents.BrowserEvents.Utils (goUpParent)
import ViewEvents.Common
    ( commonTabEvent
    , liftContinue2
    , prepareExit
    , updateFooterGuarded
    )
import ViewEvents.Copy                (copyEntryCommon)
import ViewEvents.Utils               (getSelectedEntry, isCopyable)

browserEvent :: State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State)
browserEvent =
  commonTabEvent
    ( \st e ->
        case e of
          T.VtyEvent (V.EvKey V.KEsc [])        -> handleEsc st
          T.VtyEvent (V.EvKey V.KEnter [])      -> handleEnter st
          T.VtyEvent (V.EvKey (V.KChar 'p') []) | isCopyable st -> copyPassword st
          T.VtyEvent (V.EvKey (V.KChar 'u') []) | isCopyable st -> copyUsername st
          T.VtyEvent ev                         -> M.continue $ handleNav ev st
          T.AppEvent ev                         -> liftContinue2 handleAppEvent st ev
          _                                     -> M.continue st
    )

handleEsc :: State -> T.EventM Field (T.Next State)
handleEsc st =
  case (st^.currentPath, st^.hasCopied) of
    ([], True)  -> M.continue $ prepareExit st
    ([], False) -> M.halt st
    _           -> M.continue $ goUpParent st

copyPassword :: State -> T.EventM Field (T.Next State)
copyPassword st = liftContinue2 copyEntryFromBrowser st CopyPassword

copyUsername :: State -> T.EventM Field (T.Next State)
copyUsername st = liftContinue2 copyEntryFromBrowser st CopyUsername

copyEntryFromBrowser :: State -> CopyType -> IO State
copyEntryFromBrowser st ctype =
  fromMaybe def (getSelectedEntry f st)
    where
      def = pure $ st & footer .~ str "No entry selected!"
      f entry = copyEntryCommon st entry ctype

handleNav :: V.Event -> State -> State
handleNav e st = new_st & updateFooterGuarded
  where
    new_st = st & visibleEntries %~
      case e of
        -- Keys from handleListEvent
        V.EvKey V.KUp []         -> L.listMoveUp
        V.EvKey V.KDown []       -> L.listMoveDown
        V.EvKey V.KHome []       -> listMoveToBeginning
        V.EvKey V.KEnd []        -> listMoveToEnd
        V.EvKey V.KPageDown []   -> listMovePageDown
        V.EvKey V.KPageUp []     -> listMovePageUp
        -- WASD
        V.EvKey (V.KChar 'w') [] -> L.listMoveUp
        V.EvKey (V.KChar 's') [] -> L.listMoveDown
        V.EvKey (V.KChar 'e') [] -> listMovePageDown
        V.EvKey (V.KChar 'q') [] -> listMovePageUp
        -- Vi
        V.EvKey (V.KChar 'k') [] -> L.listMoveUp
        V.EvKey (V.KChar 'j') [] -> L.listMoveDown
        V.EvKey (V.KChar 'g') [] -> listMoveToBeginning
        V.EvKey (V.KChar 'G') [] -> listMoveToEnd
        _                        -> id
    -- Default page up and down functions too fast for me
    listMovePageUp l = listMoveBy (subtract 5) l
    listMovePageDown l = listMoveBy (5 +) l
    listMoveBy f l = L.listMoveTo clamped l
      where
        clamped = clamp 0 (length $ L.listElements l) num
        num = f (fromMaybe 0 $ L.listSelected l)
    -- Not exported by Brick.Widgets.List for me
    listMoveToBeginning = L.listMoveTo 0
    listMoveToEnd l = L.listMoveTo (length $ L.listElements l) l
