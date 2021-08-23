{-# LANGUAGE OverloadedStrings #-}

module Common where

import Data.Text ( Text )
import Data.Map.Strict ((!?))
import Lens.Micro ( (^.) )
import Brick.Types (Widget)
import Brick.Markup (markup, (@?), Markup)
import Brick.AttrMap (AttrName)
import qualified Data.Vector as Vec
import qualified Brick.Focus as F
import qualified Brick.Widgets.List as L

import Types
    ( activeView,
      allEntryDetails,
      currentDir,
      currentEntryDetailName,
      focusRing,
      Field(BrowserField, PathField, PasswordField),
      State,
      View(ExitView, SearchView, EntryView, BrowserView, PasswordView) )

-- | This should only be used for running the show cmd
dirsToStr :: [String] -> String
dirsToStr = foldr (++) []

-- | This should be used for accessing any other mappings in the state
dirsToStrRoot :: [String] -> String
dirsToStrRoot x =
  case dirsToStr x of
    "" -> "."
    y -> y

annotate :: [(Text, Text)] -> Widget Field
annotate x = markup $ foldr1 (<>) (f <$> x)
  where
    f :: (Text, Text) -> Markup AttrName
    f (key, label) = (key @? "key") <> (label @? "label")

footers :: State -> Widget Field
footers st =
  annotate $ case st^.activeView of
    SearchView -> [exit, tab " focus list "]
    EntryView -> [back, username, password]
    BrowserView ->
      case st^.currentDir of
        [] -> [exit, focus_search, username, password]
        _ -> [back, focus_search, username, password]
    PasswordView ->
      case F.focusGetCurrent (st ^. focusRing) of
        Just PathField -> exit_tab_submit "password"
        Just PasswordField -> exit_tab_submit "keyfile"
        _ -> exit_tab_submit "path"
    ExitView -> [("", "")]
  where
    exit = ("Esc", " exit  ")
    tab label = ("Tab", label)
    back = ("Esc", " back  ")
    username = ("u", " copy username  ")
    password = ("p", " copy password")
    focus_search = ("Tab", " focus search  ")
    exit_tab_submit x =
      [exit, tab (" focus " <> x <> " field  "), ("Enter", " submit")]

toBrowserList :: [String] -> L.List Field String
toBrowserList xs = L.list BrowserField (Vec.fromList xs) 1

maybeGetEntryData :: State -> Maybe String
maybeGetEntryData st = do
  let dirname = dirsToStrRoot (st^.currentDir)
  entryname <- st^.currentEntryDetailName
  entriesInThisDir <- (st^.allEntryDetails) !? dirname
  entriesInThisDir !? entryname
