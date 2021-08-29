-- | Functions that all browser event module uses
-- The core functionality of show entry, enter dir, and go up parent are here
-- Plus some extra utils they use
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.BrowserEvents.Utils where

import qualified Brick.Widgets.Edit as E
import           Data.Functor       ((<&>))
import           Data.Map.Strict    ((!?))
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import           Lens.Micro         ((%~), (&), (.~), (?~), (^.))

import Common
    ( pathToStr
    , pathToStrRoot
    , maybeGetEntryData
    , toBrowserList
    )
import Types
    ( Field (SearchField)
    , State
    , View (EntryDetailsView)
    , activeView
    , allEntryDetails
    , allEntryNames
    , currentPath
    , selectedEntryName
    , searchField
    , visibleEntries
    )
import ViewEvents.Common (updateFooter)


maybeGetEntries :: State -> Maybe [Text]
maybeGetEntries st =
  (st ^. allEntryNames) !? dir
  where
    newDir = initOrDef ["."] (st ^. currentPath)
    dir = pathToStr newDir

initOrDef :: [a] -> [a] -> [a]
initOrDef d []  = d
initOrDef d [_] = d
initOrDef _ xs  = init xs

showEntryWithCache :: State -> Text -> Maybe State
showEntryWithCache st entryname =
  maybeGetEntryData st <&> showEntryInner st entryname

showEntrySuccess :: State -> Text -> Text -> State
showEntrySuccess st entry stdout =
  case maybeGetEntryData st of
    Just x  -> showEntryInner st entry x
    Nothing -> showEntryInner st entry stdout

showEntryInner :: State -> Text -> Text -> State
showEntryInner st entry details = newst
  where
    dirname = pathToStrRoot (st^.currentPath)
    f :: Maybe (M.Map Text Text) -> Maybe (M.Map Text Text)
    f (Just m) = Just $ M.insertWith (curry snd) entry details m
    f _        = Just $ M.singleton entry details
    newst = st & activeView             .~ EntryDetailsView
               & selectedEntryName ?~ entry
               & allEntryDetails        %~ M.alter f dirname
               & updateFooter
               -- Not guarded here because the countdown should only be in
               -- browser view

-- allEntryNames is updated here only, so that we can search inside the folder
enterDirSuccess :: State -> [Text] -> Text -> State
enterDirSuccess st entries_ rawDir =
  st & visibleEntries .~ toBrowserList entries_
     & allEntryNames  %~ M.insert rawDir entries_
     & searchField    .~ E.editor SearchField (Just 1) ""
     & currentPath     %~ (++ [rawDir])
     & updateFooter  -- clears any footers set when entering dir

goUpParent :: State -> State
goUpParent st =
  st & visibleEntries .~ toBrowserList entries
     & searchField    .~ E.editor SearchField (Just 1) ""
     & currentPath     %~ initOrDef []
     & updateFooter
  where
    entries = fromMaybe ["Failed to get entries!"] $ maybeGetEntries st
