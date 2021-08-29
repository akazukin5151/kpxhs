-- Functions that do simple calculations
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.Utils where

import qualified Brick.Widgets.Edit as E
import           Data.List          (partition, sort)
import           Data.Text          (Text)
import qualified Data.Text          as TT
import qualified Data.Text.Zipper   as Z
import           Lens.Micro         ((^.))
import           System.Process     (readProcessWithExitCode)

import Types
    ( CmdAction (..)
    , CmdOutput
    , Field
    , State
    , dbPathField
    , keyfileField
    , passwordField, visibleEntries
    )
import qualified Brick.Widgets.List as L
import Data.Maybe (fromMaybe)


getSelectedEntry :: (Text -> a) -> State -> Maybe a
getSelectedEntry f st = do
  (_, entry) <- L.listSelectedElement $ st ^. visibleEntries
  pure $ f entry

processStdout :: Text -> [Text]
processStdout s = dirs ++ entries_
  where
    (dirs, entries_) = partition ("/" `TT.isSuffixOf`) x
    x = sort $ TT.lines s

getCreds :: State -> (Text, Text, Text)
getCreds st = (dir, pw, kf)
  where
    dir = extractTextField $ st ^. dbPathField
    pw = extractTextField  $ st ^. passwordField
    kf = extractTextField  $ st ^. keyfileField
    extractTextField :: E.Editor Text Field -> Text
    extractTextField field =
      let res = Z.getText $ field ^. E.editContentsL in
      case res of
        []      -> ""
        (x : _) -> x

isDir :: State -> Bool
isDir st = fromMaybe False (getSelectedEntry f st)
  where
    f = (== '/') . TT.last

isGoUpToParent :: State -> Bool
isGoUpToParent st = fromMaybe False (getSelectedEntry f st)
  where
    f = (== "-- (Go up parent) --")

isCopyable :: State -> Bool
isCopyable st = not (isDir st || isGoUpToParent st)

actionToString :: CmdAction -> String
actionToString Ls   = "ls"
actionToString Clip = "clip"
actionToString Show = "show"

runCmd :: CmdAction
       -> Text    -- ^ dir
       -> [Text]  -- ^ args
       -> Text    -- ^ password
       -> Text    -- ^ keyfile path
       -> IO CmdOutput
runCmd a = _runCmdInner (actionToString a)

_runCmdInner :: String
             -> Text
             -> [Text]
             -> Text
             -> Text
             -> IO CmdOutput
_runCmdInner action dir extraArgs pw kf = do
  (code, stdout, stderr) <- readProcessWithExitCode "keepassxc-cli" args (TT.unpack pw)
  pure (code, TT.pack stdout, TT.pack stderr)
  where
    args :: [String]
    args = [action, TT.unpack dir] ++ extraArgs_
    extraArgs_ =
      TT.unpack <$> case kf of
        "" -> extraArgs
        _  -> ["-k", kf] ++ extraArgs
