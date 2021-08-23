{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.BrowserEvents (browserEvent) where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import           Brick.Util             (clamp)
import           Brick.Widgets.Core     (str)
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Map.Strict        ((!?))
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((%~), (&), (.~), (?~), (^.))
import           System.Exit            (ExitCode (ExitSuccess))

import           Common                 ( dirsToStr
                                        , dirsToStrRoot
                                        , footers
                                        , maybeGetEntryData
                                        , toBrowserList
                                        )
import           Types                  ( Action (Ls, Show)
                                        , CopyType (..)
                                        , Field (SearchField)
                                        , State
                                        , View (EntryView)
                                        , activeView
                                        , allEntryDetails
                                        , allEntryNames
                                        , currentDir
                                        , currentEntryDetailName
                                        , footer
                                        , hasCopied
                                        , searchField
                                        , visibleEntries
                                        )
import           ViewEvents.Common      ( commonTabEvent
                                        , copyEntryCommon
                                        , getCreds
                                        , prepareExit
                                        , processInput
                                        , runCmd
                                        )

browserEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
browserEvent =
  commonTabEvent
    ( \st e ->
        case e of
          V.EvKey V.KEsc [] -> handleEsc st
          V.EvKey V.KEnter [] ->
            if isDir st
              then liftIO (enterDir st) >>= M.continue
              else liftIO (showEntry st) >>= M.continue
          V.EvKey (V.KChar 'p') [] ->
            liftIO (copyEntryFromBrowser st CopyPassword) >>= M.continue
          V.EvKey (V.KChar 'u') [] ->
            liftIO (copyEntryFromBrowser st CopyUsername) >>= M.continue
          ev -> M.continue $ handleNav ev st
    )

handleEsc :: State -> T.EventM Field (T.Next State)
handleEsc st =
  case (st^.currentDir, st^.hasCopied) of
    ([], True)  -> M.continue $ prepareExit st
    ([], False) -> M.halt st
    _           -> M.continue $ goUpParent st

-- If there is "go up to parent" then check for that first
isDir :: State -> Bool
isDir st = fromMaybe False (processSelected f st)
  where
    f entry = last entry == '/'

processSelected :: (String -> a) -> State -> Maybe a
processSelected f st = do
  (_, entry) <- L.listSelectedElement $ st ^. visibleEntries
  pure $ f entry

enterDir :: State -> IO State
enterDir st = fromMaybe def (processSelected f st)
  where
    def = pure $ st & footer .~ str "No directory selected!"
    f entry = enterDirTryCache st entry ((st ^. allEntryNames) !? entry)

enterDirTryCache :: State -> String -> Maybe [String] -> IO State
enterDirTryCache st rawDir (Just x) = pure $ enterDirSuccess st x rawDir
enterDirTryCache st rawDir Nothing = do
  (code, stdout, stderr) <- runCmd Ls dbPathField_ [concatedDir] pw kf
  pure $ enterDirTryCmd st stdout stderr rawDir code
  where
    (dbPathField_, pw, kf) = getCreds st
    concatedDir = (dirsToStr (st ^. currentDir)) ++ rawDir

enterDirTryCmd :: State -> String -> String -> String -> ExitCode -> State
enterDirTryCmd st stdout _ rawDir ExitSuccess =
  enterDirSuccess st ("-- (Go up parent) --" : processInput stdout) rawDir
enterDirTryCmd st _ stderr _ _ = st & footer .~ str stderr

-- allEntryNames is updated here only, so that we can search inside the folder
enterDirSuccess :: State -> [String] -> String -> State
enterDirSuccess st entries_ rawDir =
  newst & footer .~ footers st
    where
      newst = st & visibleEntries .~ toBrowserList entries_
                 & allEntryNames %~ Map.insert rawDir entries_
                 & searchField .~ E.editor SearchField (Just 1) ""
                 & currentDir %~ (++ [rawDir])

showEntry :: State -> IO State
showEntry st = fromMaybe def $ processSelected (showEntryInner st) st
  where
    def = pure $ st & footer .~ str "No entry or directory selected!"

showEntryInner :: State -> String -> IO State
showEntryInner st entry =
  if entry == "-- (Go up parent) --"
    then pure $ goUpParent st
    else showEntryTryCache st entry

goUpParent :: State -> State
goUpParent st =
  st & visibleEntries .~ toBrowserList entries
     & searchField .~ E.editor SearchField (Just 1) ""
     & currentDir %~ initOrDef []
  where
    entries = fromMaybe ["Failed to get entries!"] $ maybeGetEntries st

maybeGetEntries :: State -> Maybe [String]
maybeGetEntries st =
  (st ^. allEntryNames) !? dir
  where
    newDir = initOrDef ["."] (st ^. currentDir)
    dir = dirsToStr newDir

initOrDef :: [a] -> [a] -> [a]
initOrDef d []  = d
initOrDef d [_] = d
initOrDef _ xs  = init xs


showEntryTryCache :: State -> String -> IO State
showEntryTryCache st entryname = fromMaybe def (showEntryWithCache st entryname)
  where
    def = showEntryCmd st entryname

showEntryWithCache :: State -> String -> Maybe (IO State)
showEntryWithCache st entryname = do
  details <- maybeGetEntryData st
  pure $ pure $ showEntrySuccess st entryname details

showEntryCmd :: State -> String -> IO State
showEntryCmd st entry = do
  let (dir, pw, kf) = getCreds st
  (code, stdout, stderr) <- runCmd Show dir [entry] pw kf
  case code of
    ExitSuccess -> pure $ showEntrySuccess st entry stdout
    _           -> pure $ st & footer .~ str stderr

showEntrySuccess :: State -> String -> String -> State
showEntrySuccess st entry details =
  newst & footer .~ footers newst
       where
         dirname = dirsToStrRoot (st^.currentDir)
         f :: Maybe (Map.Map String String) -> Maybe (Map.Map String String)
         f (Just m) = Just $ Map.insertWith (curry snd) entry details m
         f _        = Just $ Map.singleton entry details
         newst = st & activeView .~ EntryView
                    & currentEntryDetailName ?~ entry
                    & allEntryDetails %~ Map.alter f dirname

copyEntryFromBrowser :: State -> CopyType -> IO State
copyEntryFromBrowser st ctype =
  fromMaybe def (processSelected f st)
    where
      def = pure $ st & footer .~ str "No entry selected!"
      f entry = copyEntryCommon st entry ctype

handleNav :: V.Event -> State -> State
handleNav e st =
  st & visibleEntries %~ case e of
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
  where
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
