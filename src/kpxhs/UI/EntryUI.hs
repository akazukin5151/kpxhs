{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module UI.EntryUI (drawEntryDetails) where

import Data.Maybe
import qualified Data.Text as TT
import Lens.Micro
import Brick.Types (Widget)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Table
import Brick.Widgets.Core
  ( str,
    txt,
    vBox,
  )

import Types
import Common


drawEntryDetails :: State -> [Widget Field]
drawEntryDetails st =
  [C.center $ vBox $ fromMaybe def (drawEntryDetailsInner st)]
  where
    def = [C.hCenter $ str "Failed to get entry!"]

drawEntryDetailsInner :: State -> Maybe [Widget Field]
drawEntryDetailsInner st = do
  entryData <- maybeGetEntryData st
  let tableWidget = drawTable $ TT.pack entryData
  pure [ C.hCenter $ renderTable $ table tableWidget,
         C.hCenter $ st ^. footer ]

drawTable :: TT.Text -> [[Widget Field]]
drawTable text_ = restRows ++ notesRow
  where
    -- Do not split by ": " on notes
    [rest, notes] = TT.splitOn "Notes: " text_
    xs = TT.splitOn ": " <$> TT.lines rest
    restRows = (txt . replaceEmpty <$>) <$> xs
    notesRow = [[txt "Notes", txt $ replaceEmpty notes]]

replaceEmpty :: TT.Text -> TT.Text
replaceEmpty s =
  case s of
    "" -> "-"
    x -> x

