{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.SearchEvents (searchEvent) where

import qualified Brick.Main         as M
import qualified Brick.Types        as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Data.Map.Strict    ((!?))
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Text          (Text, isInfixOf, toLower)
import qualified Graphics.Vty       as V
import           Lens.Micro         ((%~), (&), (.~), (^.))

import Common            (toBrowserList)
import Types
    ( Field
    , State
    , allEntryNames
    , currentDir
    , hasCopied
    , searchField
    , visibleEntries
    )
import ViewEvents.Common (commonTabEvent, prepareExit)


searchEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
searchEvent =
  commonTabEvent
    ( \st e ->
        case e of
          T.VtyEvent (V.EvKey V.KEsc []) -> handleEsc st
          T.VtyEvent ev                  -> M.continue =<< handleSearch st ev
          _                              -> M.continue st
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
      f :: L.List Field Text -> L.List Field Text
      f _ = toBrowserList $ filter g entries
      g :: Text -> Bool
      g entry = newStr `isInfixOf` toLower entry
      newSt = updatedSt & visibleEntries %~ f
  pure newSt

-- Adapted from the definition of last in Prelude
dirsToCurrent :: [Text] -> Text
dirsToCurrent []     = "."
dirsToCurrent [x]    = x
dirsToCurrent (_:xs) = dirsToCurrent xs
