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
    ( Action (..)
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
    f entry = TT.last entry == '/'

isGoUpToParent :: State -> Bool
isGoUpToParent st = fromMaybe False (getSelectedEntry f st)
  where
    f entry = entry == "-- (Go up parent) --"

isCopyable :: State -> Bool
isCopyable st = not (isDir st || isGoUpToParent st)

runCmd :: Action
       -> Text
       -> [Text]
       -> Text
       -> Text
       -> IO CmdOutput
runCmd Ls dir args pw kf   = _runCmdInner "ls" dir args pw kf
runCmd Clip dir args pw kf = _runCmdInner "clip" dir args pw kf
runCmd Show dir args pw kf = _runCmdInner "show" dir args pw kf

_runCmdInner :: Text
             -> Text
             -> [Text]
             -> Text
             -> Text
             -> IO CmdOutput
_runCmdInner action dir extraArgs pw kf = do
  (e, a, b) <- readProcessWithExitCode "keepassxc-cli" args (TT.unpack pw)
  pure (e, TT.pack a, TT.pack b)
  where
    args :: [String]
    args = TT.unpack <$> [action, dir] ++ extraArgs_
    extraArgs_ = case kf of
                   "" -> extraArgs
                   _  -> ["-k", kf] ++ extraArgs
