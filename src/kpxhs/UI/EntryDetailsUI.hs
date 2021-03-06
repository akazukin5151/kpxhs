{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.EntryDetailsUI (drawEntryDetails) where

import           Brick.Types          (Widget)
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (str, txt, vBox)
import           Brick.Widgets.Table  (renderTable, table)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as TT
import           Lens.Micro           ((^.))

import Common (maybeGetEntryData)
import Types  (Field, State, footer)


drawEntryDetails :: State -> [Widget Field]
drawEntryDetails st =
  [ C.center $ vBox $ fromMaybe def (drawEntryDetailsInner st) ]
  where
    def = [ C.hCenter $ str "Failed to get entry!" ]

drawEntryDetailsInner :: State -> Maybe [Widget Field]
drawEntryDetailsInner st = do
  entryData <- maybeGetEntryData st
  let tableWidget = drawTable entryData
  pure [ C.hCenter $ renderTable $ table tableWidget,
         C.hCenter $ st ^. footer ]

drawTable :: Text -> [[Widget Field]]
drawTable raw =
  case TT.splitOn "Notes: " raw of
    [rest, notes] -> drawTableWithNotes rest notes
    _             -> drawTableWithoutNotes raw

drawTableWithNotes :: Text -> Text -> [[Widget Field]]
drawTableWithNotes rest notes = restRows ++ notesRow
  where
    restRows = drawTableWithoutNotes rest
    -- To avoid splitting by ": " inside note contents
    notesRow = [[ txt "Notes", txt $ replaceEmpty notes ]]

-- For some reason the Notes section is missing
drawTableWithoutNotes :: Text -> [[Widget Field]]
drawTableWithoutNotes raw = rows
  where
    xs = TT.splitOn ": " <$> TT.lines raw
    rows = (txt . replaceEmpty <$>) <$> xs

replaceEmpty :: Text -> Text
replaceEmpty s =
  case s of
    "" -> "-"
    x  -> x
