module ViewEvents.EntryEvents (entryDetailsEvent) where

import Lens.Micro
import Control.Monad.IO.Class
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T

import Common
import Types
import ViewEvents.Common

entryDetailsEvent :: State
                  -> T.BrickEvent Field e
                  -> T.EventM Field (T.Next State)
entryDetailsEvent st (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.continue $ returnToBrowser st
    V.EvKey (V.KChar 'p') [] ->
      liftIO (copyEntryFromDetails st CopyPassword) >>= M.continue
    V.EvKey (V.KChar 'u') [] ->
      liftIO (copyEntryFromDetails st CopyUsername) >>= M.continue
    _ -> M.continue st
entryDetailsEvent st _ = M.continue st

returnToBrowser :: State -> State
returnToBrowser st =
  newst & footer .~ footers newst
    where
      newst = st & activeView .~ BrowserView
