{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.SearchEvents (searchEvent) where

import qualified Brick.Main         as M
import qualified Brick.Types        as T
import           Brick.Widgets.Core (txt)
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
    , searchField
    , visibleEntries, footer, View(BrowserView), activeView
    )
import ViewEvents.Common (commonTabEvent, updateFooterGuarded)


searchEvent :: State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State)
searchEvent =
  commonTabEvent
    ( \st e ->
        case e of
          T.VtyEvent (V.EvKey V.KEsc []) -> M.continue $ exitSearch st
          T.VtyEvent ev                  -> M.continue =<< handleSearch st ev
          _                              -> M.continue st
    )

exitSearch :: State -> State
exitSearch st =
  st & activeView .~ BrowserView
     & updateFooterGuarded

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
