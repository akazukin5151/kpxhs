{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.Common where

import System.Exit
import Data.Maybe
import Lens.Micro
import Graphics.Vty
import qualified Graphics.Vty as V
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Data.Text as TT

import Common
import Types


prepareExit :: State -> State
prepareExit st =
  st & previousView .~ (st^.activeView)
     & activeView .~ ExitView

copyEntryFromDetails :: State -> CopyType -> IO State
copyEntryFromDetails st ctype = fromMaybe def (maybeCopy st ctype)
  where
    def = pure $ st & footer .~ "Failed to get entry name or details!"

maybeCopy :: State -> CopyType -> Maybe (IO State)
maybeCopy st ctype = do
  entryData <- maybeGetEntryData st
  -- Assumes that the title is always the first row
  let [_, entry] = TT.splitOn "Title: " $ TT.pack $ head $ lines entryData
  pure $ copyEntryCommon st (TT.unpack entry) ctype

copyEntryCommon :: State -> String -> CopyType -> IO State
copyEntryCommon st entry ctype = do
  let (dir, pw, kf) = getCreds st
  let attr = copyTypeToStr ctype
  (code, _, stderr) <- runCmd Clip dir [entry, "-a", attr] pw kf
  pure $ case code of
    ExitSuccess -> st
                   & footer .~ (show attr ++ " copied to clipboard!")
                   & hasCopied .~ True
    _ -> st & footer .~ stderr

copyTypeToStr :: CopyType -> String
copyTypeToStr ctype =
  case ctype of
    CopyUsername -> "username"
    _ -> "password"

commonTabEvent :: (State -> Event -> T.EventM Field (T.Next State))
               -> State
               -> T.BrickEvent Field e
               -> T.EventM Field (T.Next State)
commonTabEvent fallback st (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar '\t') [] ->
      M.continue $ handleTab st focusNext (st ^. activeView)
    V.EvKey V.KBackTab [] ->
      M.continue $ handleTab st focusPrev (st ^. activeView)
    _ -> fallback st e
commonTabEvent _ st _ = M.continue st

handleTab :: State -> (State -> View -> State) -> View -> State
handleTab st f BrowserView = f st SearchView
handleTab st f _ = f st BrowserView

focus :: (F.FocusRing Field -> F.FocusRing Field) -> State -> View -> State
focus f st view =
  newst & footer .~ footers newst
    where
      newst = st & focusRing %~ f
                 & activeView .~ view

focusNext :: State -> View -> State
focusNext = focus F.focusNext

focusPrev :: State -> View -> State
focusPrev = focus F.focusPrev
