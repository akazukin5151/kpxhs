{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Brick.AttrMap      (AttrName)
import qualified Brick.Focus        as F
import           Brick.Markup       (Markup, markup, (@?))
import           Brick.Types        (Widget)
import qualified Brick.Widgets.List as L
import           Data.Map.Strict    ((!?))
import           Data.Text          (Text)
import qualified Data.Text          as TT
import qualified Data.Vector        as Vec
import           Lens.Micro         ((^.))

import           Types              ( Field (BrowserField, PasswordField, PathField)
                                    , State
                                    , View ( BrowserView
                                           , EntryView
                                           , ExitView
                                           , PasswordView
                                           , SearchView )
                                    , activeView
                                    , allEntryDetails
                                    , currentDir
                                    , currentEntryDetailName
                                    , focusRing
                                    )

-- | This should only be used for running the show cmd
dirsToStr :: [Text] -> Text
dirsToStr = TT.concat

-- | This should be used for accessing any other mappings in the state
dirsToStrRoot :: [Text] -> Text
dirsToStrRoot x =
  case dirsToStr x of
    "" -> "."
    y  -> y

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
        _  -> [back, focus_search, username, password]
    PasswordView ->
      case F.focusGetCurrent (st ^. focusRing) of
        Just PathField     -> exit_tab_submit "password"
        Just PasswordField -> exit_tab_submit "keyfile"
        _                  -> exit_tab_submit "path"
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

toBrowserList :: [Text] -> L.List Field Text
toBrowserList xs = L.list BrowserField (Vec.fromList xs) 1

maybeGetEntryData :: State -> Maybe Text
maybeGetEntryData st = do
  let dirname = dirsToStrRoot (st^.currentDir)
  entryname <- st^.currentEntryDetailName
  entriesInThisDir <- (st^.allEntryDetails) !? dirname
  entriesInThisDir !? entryname
