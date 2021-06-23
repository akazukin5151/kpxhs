module Common where

import Data.Map.Strict ((!?))
import Lens.Micro
import qualified Data.Vector as Vec
import qualified Brick.Focus as F
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
    ExitView -> ""

toBrowserList :: [String] -> L.List Field String
toBrowserList xs = L.list BrowserField (Vec.fromList xs) 1

maybeGetEntryData :: State -> Maybe String
maybeGetEntryData st = do
  let dirname = dirsToStrRoot (st^.currentDir)
  entryname <- st^.currentEntryDetailName
  entriesInThisDir <- (st^.allEntryDetails) !? dirname
  entriesInThisDir !? entryname

