module Common where

import Data.List
import Data.Map.Strict ((!?))
import Lens.Micro
import qualified Data.Vector as Vec
import qualified Data.Text.Zipper as Z hiding (textZipper)
import System.Exit
import System.Process
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L

import Types

-- | This should only be used for running the show cmd
dirsToStr :: [String] -> String
dirsToStr = foldr (++) []

-- | This should be used for accessing any other mappings in the state
dirsToStrRoot :: [String] -> String
dirsToStrRoot x =
  case dirsToStr x of
    "" -> "."
    y -> y

hidePassword :: [String] -> String
hidePassword xs = replicate (length (unlines xs) - 1) '*'

initOrDef :: [a] -> [a] -> [a]
initOrDef d [] = d
initOrDef d [_] = d
initOrDef _ xs = init xs

processInput :: String -> [String]
processInput s = dirs ++ entries_
  where
    (dirs, entries_) = partition ("/" `isSuffixOf`) $ sort $ lines s

footers :: State -> String
footers st =
  case st^.activeView of
    SearchView -> "Esc: exit, Tab: focus list"
    EntryView -> "Esc: back, u: copy username, p: copy password"
    BrowserView ->
      case st^.currentDir of
        [] -> "Esc: exit, Tab: focus search, u: copy username, p: copy password"
        _ -> "Esc: back, Tab: focus search, u: copy username, p: copy password"
    PasswordView ->
      case F.focusGetCurrent (st ^. focusRing) of
        Just PathField -> "Esc: exit, Tab: focus password field, Enter: submit"
        Just PasswordField -> "Esc: exit, Tab: focus keyfile field, Enter: submit"
        _ -> "Esc: exit, Tab: focus path field, Enter: submit"

toBrowserList :: [String] -> L.List Field String
toBrowserList xs = L.list BrowserField (Vec.fromList xs) 1

valid :: State -> Bool
valid st = f $ getCreds st
  where
    f (a, b, _) = a /= "" && b /= ""

getCreds :: State -> (String, String, String)
getCreds st = (dir, pw, kf)
  where
    dir = extractTextField $ st ^. dbPathField
    pw = extractTextField $ st ^. passwordField
    kf = extractTextField $ st ^. keyfileField

extractTextField :: E.Editor String Field -> String
extractTextField field =
  let res = Z.getText $ field ^. E.editContentsL in
  case res of
    [] -> ""
    (x : _) -> x

maybeGetEntryData :: State -> Maybe String
maybeGetEntryData st = do
  let dirname = dirsToStrRoot (st^.currentDir)
  entryname <- st^.currentEntryDetailName
  entriesInThisDir <- (st^.allEntryDetails) !? dirname
  entriesInThisDir !? entryname

runCmd :: Action
       -> String
       -> [String]
       -> String
       -> String
       -> IO (ExitCode, String, String)
runCmd Ls dir args pw kf = runCmdInner "ls" dir args pw kf
runCmd Clip dir args pw kf = runCmdInner "clip" dir args pw kf
runCmd Show dir args pw kf = runCmdInner "show" dir args pw kf

runCmdInner :: String
            -> String
            -> [String]
            -> String
            -> String
            -> IO (ExitCode, String, String)
runCmdInner action dir extraArgs pw kf =
  readProcessWithExitCode "keepassxc-cli" args pw
  where
    args = [action, dir, "--quiet"] ++ extraArgs_
    extraArgs_ = case kf of
                   "" -> extraArgs
                   _ -> ["-k", kf] ++ extraArgs
