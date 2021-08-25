{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Brick.AttrMap        (AttrName)
import qualified Brick.Focus          as F
import           Brick.Markup         (Markup, markup, (@?))
import           Brick.Types          (Widget)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List   as L
import           Data.Map.Strict      ((!?))
import           Data.Text            (Text)
import qualified Data.Text            as TT
import qualified Data.Vector          as Vec
import           Lens.Micro           ((^.))

import Types
    ( ExitDialog (Cancel, Clear, Exit)
    , Field (BrowserField, PasswordField, PathField)
    , State
    , allEntryDetails
    , currentDir
    , currentEntryDetailName
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

exit :: (Text, Text)
exit = ("Esc", " exit  ")

tab :: Text -> (Text, Text)
tab label = ("Tab", label)

initialFooter :: F.FocusRing Field -> [(Text, Text)]
initialFooter fr =
  case F.focusGetCurrent fr of
    Just PathField     -> exit_tab_submit "password"
    Just PasswordField -> exit_tab_submit "keyfile"
    _                  -> exit_tab_submit "path"
  where
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

defaultDialog :: D.Dialog ExitDialog
defaultDialog = D.dialog Nothing (Just (0, defaultDialogChoices)) 60

defaultDialogChoices :: [(String, ExitDialog)]
defaultDialogChoices =
  [ ("Clear and exit", Clear)
  , ("Just exit", Exit)
  , ("Do not exit", Cancel)
  ]
