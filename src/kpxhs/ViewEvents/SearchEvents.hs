{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.SearchEvents (searchEvent) where

import           Brick              (txt)
import qualified Brick.Main         as M
import qualified Brick.Types        as T
import qualified Brick.Widgets.Edit as E
import           Data.Map.Strict    ((!?))
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Text          (Text, isInfixOf, toLower)
import qualified Graphics.Vty       as V
import           Lens.Micro         ((&), (.~), (^.))

import Common            (toBrowserList)
import Types
    ( Event
    , Field
    , State
    , allEntryNames
    , currentPath
    , hasCopied
    , searchField
    , visibleEntries, footer
    )
import ViewEvents.Common (commonTabEvent, prepareExit)


searchEvent :: State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State)
searchEvent =
  commonTabEvent
    ( \st e ->
        case e of
          T.VtyEvent (V.EvKey V.KEsc [])    -> handleEsc st
          T.VtyEvent ev                     -> M.continue =<< handleSearch st ev
          _                                 -> M.continue st
    )

handleEsc :: State -> T.EventM Field (T.Next State)
handleEsc st =
  -- Esc on search will attempt an exit, even if Browser is inside dirs
  if st^.hasCopied
    then M.continue $ prepareExit st
    else M.halt st

handleSearch :: State -> V.Event -> T.EventM Field State
handleSearch st e = do
  field <- E.handleEditorEvent e (st ^. searchField)
  let updatedSt = st & searchField .~ field
      searchStr = toLower $ fromMaybe "" $ listToMaybe $ E.getEditContents field
      f entry = searchStr `isInfixOf` toLower entry
      g = case (st ^. allEntryNames) !? theDir of
            Nothing -> footer         .~ txt "Failed to get entries!"
            Just x  -> visibleEntries .~ toBrowserList (filter f x)
  pure $ updatedSt & g
  where
    theDir = dirsToCurrent $ st ^. currentPath


-- Adapted from the definition of last in Prelude
dirsToCurrent :: [Text] -> Text
dirsToCurrent []     = "."
dirsToCurrent [x]    = x
dirsToCurrent (_:xs) = dirsToCurrent xs
