{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.EntryEvents (entryDetailsEvent) where

import qualified Brick.Main                as M
import qualified Brick.Types               as T
import           Brick.Widgets.Core        (str)
import qualified Brick.Widgets.ProgressBar as P
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as TT
import qualified Graphics.Vty              as V
import           Lens.Micro                ((&), (.~), (^.))

import Common            (maybeGetEntryData)
import Types
    ( CopyType (CopyPassword, CopyUsername)
    , Event (ClearClipCount, Copying)
    , Field
    , State
    , View (BrowserView)
    , activeView
    , clearTimeout
    , currentCountdown
    , footer
    )
import ViewEvents.Common
    ( copyEntryCommon
    , handleClipCount
    , handleCopy
    , liftContinue2
    , mkCountdownLabel
    , updateFooterGuarded
    )

entryDetailsEvent :: State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State)
entryDetailsEvent st (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc []        -> M.continue $ returnToBrowser st
    V.EvKey (V.KChar 'p') [] -> liftContinue2 copyEntryFromDetails st CopyPassword
    V.EvKey (V.KChar 'u') [] -> liftContinue2 copyEntryFromDetails st CopyUsername
    _                        -> M.continue st
entryDetailsEvent st (T.AppEvent (ClearClipCount count)) =
  liftContinue2 handleClipCount st count
entryDetailsEvent st (T.AppEvent (Copying ev)) = liftContinue2 handleCopy st ev
entryDetailsEvent st _ = M.continue st

returnToBrowser :: State -> State
returnToBrowser st =
  st & activeView .~ BrowserView
     & f
  where
    toCount :: Float -> Int
    toCount x = round $ x * fromIntegral (st^.clearTimeout)
    f = case st^.currentCountdown of
      Just x  -> footer .~ P.progressBar (mkCountdownLabel $ toCount x) x
      Nothing -> updateFooterGuarded

copyEntryFromDetails :: State -> CopyType -> IO State
copyEntryFromDetails st ctype = fromMaybe def (maybeCopy st ctype)
  where
    def = pure $ st & footer .~ str "Failed to get entry name or details!"

maybeCopy :: State -> CopyType -> Maybe (IO State)
maybeCopy st ctype = do
  entryData <- maybeGetEntryData st
  -- Assumes that the title is always the first row
  let splitted = TT.splitOn "Title: " $ head $ TT.lines entryData
  case splitted of
    [_, entry] -> Just $ copyEntryCommon st entry ctype
    _          -> Nothing
