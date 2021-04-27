module ViewEvents.SearchEvents (searchEvent) where

import Data.Char
import Data.List
import Data.Map.Strict ((!?))
import Data.Maybe
import Lens.Micro
import Graphics.Vty
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L

import Common
import Types
import ViewEvents.Common

searchEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
searchEvent = commonTabEventWithEsc (\st e -> M.continue =<< handleSearch st e)

handleSearch :: State -> Event -> T.EventM Field State
handleSearch st e = do
  newB <- E.handleEditorEvent e (st ^. searchField)
  let updatedSt = st & searchField .~ newB
      newStr = fromMaybe "" $ listToMaybe $ E.getEditContents newB
      newDir = dirsToCurrent $ st ^. currentDir
      maybeEntries = (st ^. allEntryNames) !? newDir
      entries = fromMaybe ["Failed to get entries!"] maybeEntries
      f :: L.List Field String -> L.List Field String
      f _ = toBrowserList $ filter g entries
      g :: String -> Bool
      g entry = newStr `isInfixOf` (toLower <$> entry)
      newSt = updatedSt & visibleEntries %~ f
  pure newSt

dirsToCurrent :: [String] -> String
dirsToCurrent [] = "."
dirsToCurrent x = last x
