{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.EntryEvents (entryDetailsEvent) where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import           Brick.Widgets.Core     (str)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as TT
import qualified Graphics.Vty           as V
import           Lens.Micro             ((&), (.~))

import           Common                 (footers, maybeGetEntryData)
import           ViewEvents.Common      (copyEntryCommon, liftContinue)
import           Types                  ( CopyType (..)
                                        , Field
                                        , State
                                        , View (BrowserView)
                                        , activeView
                                        , footer
                                        )

entryDetailsEvent :: State
                  -> T.BrickEvent Field e
                  -> T.EventM Field (T.Next State)
entryDetailsEvent st (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc []        -> M.continue $ returnToBrowser st
    V.EvKey (V.KChar 'p') [] -> liftContinue copyEntryFromDetails st CopyPassword
    V.EvKey (V.KChar 'u') [] -> liftContinue copyEntryFromDetails st CopyUsername
    _                        -> M.continue st
entryDetailsEvent st _ = M.continue st

returnToBrowser :: State -> State
returnToBrowser st =
  newst & footer .~ footers newst
    where
      newst = st & activeView .~ BrowserView

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
