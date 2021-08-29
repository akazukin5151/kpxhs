-- | Functions that run a shell command in the background ("Fork")
-- (After trying the cache first)
-- The output of the shell command will be handled later,
-- asynchronously, by functions in ViewEvents.BrowserEvents.Event
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.BrowserEvents.Fork (handleEnter) where

import           Brick              (txt)
import           Brick.BChan        (writeBChan)
import qualified Brick.Types        as T
import           Brick.Widgets.Core (str)
import           Control.Concurrent (forkIO)
import           Control.Monad      (void)
import           Data.Map.Strict    ((!?))
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import           Lens.Micro         ((&), (.~), (?~), (^.))

import Common
    ( dirsToStr
    )
import Types
    ( CmdAction (Ls, Show)
    , Event (EnterDir, ShowEntry)
    , Field
    , State
    , allEntryNames
    , chan
    , currentDir
    , selectedEntryName
    , footer
    )
import ViewEvents.Common (liftContinue1)
import ViewEvents.Utils  (getCreds, getSelectedEntry, isDir, runCmd)
import ViewEvents.BrowserEvents.Utils (enterDirSuccess, showEntryWithCache, goUpParent)

handleEnter :: State -> T.EventM Field (T.Next State)
handleEnter st = liftContinue1 f st
  where
    f = if isDir st then enterDirFork else showEntryFork

showEntryFork :: State -> IO State
showEntryFork st = fromMaybe def (getSelectedEntry f st)
  where
    def = pure $ st & footer .~ str "No entry selected!"
    f "-- (Go up parent) --" = pure $ goUpParent st
    f entry                  = fetchEntryInBackground st entry

fetchEntryInBackground :: State -> Text -> IO State
fetchEntryInBackground st entry = maybe def pure (showEntryWithCache newst entry)
  where
    newst = st & selectedEntryName ?~ entry
    (dir, pw, kf) = getCreds newst
    bg_cmd = do
      (code, stdout, stderr) <- runCmd Show dir [entry] pw kf
      writeBChan (newst^.chan) $ ShowEntry entry (code, stdout, stderr)
    def = do
      void $ forkIO bg_cmd
      pure $ newst & footer .~ txt "Fetching..."


enterDirFork :: State -> IO State
enterDirFork st = fromMaybe def (getSelectedEntry f st)
  where
    def = pure $ st & footer .~ str "No directory selected!"
    f = fetchDirInBackground st

fetchDirInBackground :: State -> Text -> IO State
fetchDirInBackground st entry  =
  case (st ^. allEntryNames) !? entry of
    Just x -> pure $ enterDirSuccess st x entry
    Nothing -> do
      void $ forkIO bg_cmd
      pure $ st & footer .~ txt "Fetching..."
  where
    (dbPathField_, pw, kf) = getCreds st
    concatedDir = dirsToStr (st ^. currentDir) <> entry
    bg_cmd = do
      (code, stdout, stderr) <- runCmd Ls dbPathField_ [concatedDir] pw kf
      writeBChan (st^.chan) $ EnterDir entry (code, stdout, stderr)
