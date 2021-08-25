{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.BrowserEvents (browserEvent) where

import           Brick.BChan        (writeBChan)
import qualified Brick.Main         as M
import qualified Brick.Types        as T
import           Brick.Util         (clamp)
import           Brick.Widgets.Core (str, txt)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Control.Concurrent (forkIO)
import           Control.Monad      (void)
import           Data.Map.Strict    ((!?))
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Graphics.Vty       as V
import           Lens.Micro         ((%~), (&), (.~), (?~), (^.))
import           System.Exit        (ExitCode (ExitSuccess))

import Common
    ( dirsToStr
    , dirsToStrRoot
    , maybeGetEntryData
    , toBrowserList
    )
import Types
    ( Action (Ls, Show)
    , CmdOutput
    , CopyType (CopyPassword, CopyUsername)
    , Event (ClearClipCount, Copying, EnterDir, ShowEntry)
    , Field (SearchField)
    , State
    , View (EntryView)
    , activeView
    , allEntryDetails
    , allEntryNames
    , chan
    , currentDir
    , currentEntryDetailName
    , footer
    , hasCopied
    , searchField
    , visibleEntries
    )
import ViewEvents.Common
    ( commonTabEvent
    , copyEntryCommon
    , getCreds
    , handleClipCount
    , handleCopy
    , isCopyable
    , isDir
    , liftContinue1
    , liftContinue2
    , prepareExit
    , processInput
    , processSelected
    , runCmd
    , updateFooter
    )

browserEvent :: State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State)
browserEvent =
  commonTabEvent
    ( \st e ->
        case e of
          T.VtyEvent (V.EvKey V.KEsc [])        -> handleEsc st
          T.VtyEvent (V.EvKey V.KEnter [])      -> handleEnter st
          T.VtyEvent (V.EvKey (V.KChar 'p') []) | isCopyable st ->
            liftContinue2 copyEntryFromBrowser st CopyPassword
          T.VtyEvent (V.EvKey (V.KChar 'u') []) | isCopyable st ->
            liftContinue2 copyEntryFromBrowser st CopyUsername
          T.VtyEvent ev                         -> M.continue $ handleNav ev st
          T.AppEvent ev                         -> liftContinue2 handleAppEvent st ev
          _                                     -> M.continue st
    )

handleEsc :: State -> T.EventM Field (T.Next State)
handleEsc st =
  case (st^.currentDir, st^.hasCopied) of
    ([], True)  -> M.continue $ prepareExit st
    ([], False) -> M.halt st
    _           -> M.continue $ goUpParent st


-- | This tree of functions will run a shell command in the background
-- (After trying the cache first)
-- The output of the shell command will be handled later,
-- asynchronously, by handleAppEvent
handleEnter :: State -> T.EventM Field (T.Next State)
handleEnter st = liftContinue1 f st
  where
    f = if isDir st then enterDirFork else showEntryFork

showEntryFork :: State -> IO State
showEntryFork st = fromMaybe def (processSelected f st)
  where
    def = pure $ st & footer .~ str "No entry selected!"
    f "-- (Go up parent) --" = pure $ goUpParent st
    f entry                  = fetchEntryInBackground st entry

fetchEntryInBackground :: State -> Text -> IO State
fetchEntryInBackground st entry = fromMaybe def (showEntryWithCache newst entry)
  where
    newst = st & currentEntryDetailName ?~ entry
    (dir, pw, kf) = getCreds newst
    bg_cmd = do
      (code, stdout, stderr) <- runCmd Show dir [entry] pw kf
      writeBChan (newst^.chan) $ ShowEntry entry (code, stdout, stderr)
    def = do
      void $ forkIO bg_cmd
      pure $ newst & footer .~ txt "Fetching..."


enterDirFork :: State -> IO State
enterDirFork st = fromMaybe def (processSelected f st)
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

-- | This tree of functions handles the completion of a shell command
handleAppEvent :: State -> Event -> IO State
handleAppEvent st (ShowEntry entry out)  = pure $ handleShowEntryEvent st entry out
handleAppEvent st (EnterDir entry out)   = pure $ handleEnterDirEvent st entry out
handleAppEvent st (ClearClipCount count) = handleClipCount st count
handleAppEvent st (Copying e)            = handleCopy st e
handleAppEvent st _                      = pure st

handleEnterDirEvent :: State -> Text -> CmdOutput -> State
handleEnterDirEvent st entry (ExitSuccess, stdout, _) =
  enterDirSuccess st ("-- (Go up parent) --" : processInput stdout) entry
handleEnterDirEvent st _ (_, _, stderr) =
  st & footer .~ txt stderr

handleShowEntryEvent :: State -> Text -> CmdOutput -> State
handleShowEntryEvent st entry (ExitSuccess, stdout, _) = showEntrySuccess st entry stdout
handleShowEntryEvent st _     (_, _, stderr)           = st & footer .~ txt stderr

showEntrySuccess :: State -> Text -> Text -> State
showEntrySuccess st entry stdout =
  case maybeGetEntryData st of
    Just x  -> showEntryInner st entry x
    Nothing -> showEntryInner st entry stdout

-- allEntryNames is updated here only, so that we can search inside the folder
enterDirSuccess :: State -> [Text] -> Text -> State
enterDirSuccess st entries_ rawDir =
  st & visibleEntries .~ toBrowserList entries_
     & allEntryNames %~ Map.insert rawDir entries_
     & searchField .~ E.editor SearchField (Just 1) ""
     & currentDir %~ (++ [rawDir])
     & updateFooter  -- clears any footers set when entering dir

goUpParent :: State -> State
goUpParent st =
  st & visibleEntries .~ toBrowserList entries
     & searchField .~ E.editor SearchField (Just 1) ""
     & currentDir %~ initOrDef []
     & updateFooter
  where
    entries = fromMaybe ["Failed to get entries!"] $ maybeGetEntries st

maybeGetEntries :: State -> Maybe [Text]
maybeGetEntries st =
  (st ^. allEntryNames) !? dir
  where
    newDir = initOrDef ["."] (st ^. currentDir)
    dir = dirsToStr newDir

initOrDef :: [a] -> [a] -> [a]
initOrDef d []  = d
initOrDef d [_] = d
initOrDef _ xs  = init xs


showEntryWithCache :: State -> Text -> Maybe (IO State)
showEntryWithCache st entryname = do
  details <- maybeGetEntryData st
  pure $ pure $ showEntryInner st entryname details

showEntryInner :: State -> Text -> Text -> State
showEntryInner st entry details = newst
  where
    dirname = dirsToStrRoot (st^.currentDir)
    f :: Maybe (Map.Map Text Text) -> Maybe (Map.Map Text Text)
    f (Just m) = Just $ Map.insertWith (curry snd) entry details m
    f _        = Just $ Map.singleton entry details
    newst = st & activeView .~ EntryView
               & currentEntryDetailName ?~ entry
               & allEntryDetails %~ Map.alter f dirname
               & updateFooter

copyEntryFromBrowser :: State -> CopyType -> IO State
copyEntryFromBrowser st ctype =
  fromMaybe def (processSelected f st)
    where
      def = pure $ st & footer .~ str "No entry selected!"
      f entry = copyEntryCommon st entry ctype

handleNav :: V.Event -> State -> State
handleNav e st = new_st & updateFooter
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
