module ViewEvents.SearchEvents (searchEvent) where

import Data.Char
import Data.List (isInfixOf)
import Data.Map.Strict ((!?))
import Data.Maybe
import Lens.Micro
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L

import Common
import Types
import ViewEvents.Common


searchEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
searchEvent =
  commonTabEvent
    ( \st e ->
        case e of
          V.EvKey V.KEsc [] -> handleEsc st
          _ -> M.continue =<< handleSearch st e
    )

handleEsc :: State -> T.EventM Field (T.Next State)
handleEsc st =
  -- Esc on search will attempt an exit, even if Browser is inside dirs
  if st^.hasCopied
    then M.continue $ prepareExit st
    else M.halt st

handleSearch :: State -> V.Event -> T.EventM Field State
handleSearch st e = do
  newB <- E.handleEditorEvent e (st ^. searchField)
  let updatedSt = st & searchField .~ newB
      newStr = fromMaybe "" $ listToMaybe $ E.getEditContents newB
      newDir = dirsToCurrent $ st ^. currentDir
      maybeEntries = (st ^. allEntryNames) !? newDir
      entries = fromMaybe ["Failed to get entries!"] maybeEntries
      f :: L.List Field String -> L.List Field String
      f _ = toBrowserList $ filter g entries
      g :: String -> Bool
      g entry = newStr `isInfixOf` (toLower <$> entry)
      newSt = updatedSt & visibleEntries %~ f
  pure newSt

-- Adapted from the definition of last in Prelude
dirsToCurrent :: [String] -> String
dirsToCurrent [] = "."
dirsToCurrent [x] = x
dirsToCurrent (_:xs) = dirsToCurrent xs
