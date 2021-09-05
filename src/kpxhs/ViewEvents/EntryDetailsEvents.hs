{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.EntryDetailsEvents (entryDetailsEvent) where

import qualified Brick.Main                as M
import qualified Brick.Types               as T
import           Brick.Widgets.Core        (str)
import qualified Brick.Widgets.ProgressBar as P
import           Data.Maybe                (fromMaybe)
import qualified Graphics.Vty              as V
import           Lens.Micro                ((&), (.~), (^.))

import Types
    ( CopyType (CopyPassword, CopyUsername)
    , Event (ClearClipCount, Copying)
    , Field
    , State
    , View (BrowserView)
    , activeView
    , clearTimeout
    , counterValue
    , footer
    , selectedEntryName
    )
import ViewEvents.Common (liftContinue2, updateFooterGuarded)
import ViewEvents.Copy
    ( copyEntryCommon
    , handleClipCount
    , handleCopy
    , mkCountdownLabel
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
    -- https://github.com/NorfairKing/haskell-dangerous-functions#fromintegral
    -- I think Int -> Float is fine because Float is larger than Int
    -- so an int shouldn't be truncated
    toCount :: Float -> Int -> Int
    toCount x t = round $ x * fromIntegral t
    f = case (st^.counterValue, st^.clearTimeout) of
      (Just x, Just t) -> footer .~ P.progressBar (mkCountdownLabel $ toCount x t) x
      _                -> updateFooterGuarded

copyEntryFromDetails :: State -> CopyType -> IO State
copyEntryFromDetails st ctype = fromMaybe def (maybeCopy st ctype)
  where
    def = pure $ st & footer .~ str "Failed to get entry name or details!"

maybeCopy :: State -> CopyType -> Maybe (IO State)
maybeCopy st ctype =
  case st ^. selectedEntryName of
    Just entry -> Just $ copyEntryCommon st entry ctype
    Nothing -> Nothing
