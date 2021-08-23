{-# LANGUAGE Rank2Types #-}

module ViewEvents.PasswordEvents (passwordEvent) where

import System.Exit ( ExitCode(ExitSuccess) )
import Lens.Micro ( Lens', (&), (%~), (.~), (^.) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Brick.Widgets.Core (str)
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as V
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E

import Common ( footers, toBrowserList )
import Types
    ( activeView,
      allEntryNames,
      dbPathField,
      focusRing,
      footer,
      keyfileField,
      passwordField,
      visibleEntries,
      Action(Ls),
      Field(..),
      State,
      View(SearchView) )
import ViewEvents.Common ( getCreds, processInput, runCmd )


valid :: State -> Bool
valid st = f $ getCreds st
  where
    f (a, b, _) = a /= "" && b /= ""

passwordEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
passwordEvent st (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
    V.EvKey V.KEnter [] | valid st -> liftIO (gotoBrowser st) >>= M.continue
    _ -> M.continue =<< handleFieldInput st e (F.focusGetCurrent (st ^. focusRing))
passwordEvent st _ = M.continue st

gotoBrowser :: State -> IO State
gotoBrowser st = do
  let (dir, pw, kf) = getCreds st
  (code, stdout, stderr) <- runCmd Ls dir [] pw kf
  case code of
    ExitSuccess -> pure $ gotoBrowserSuccess st $ processInput stdout
    _ -> pure $ st & footer .~ str stderr

gotoBrowserSuccess :: State -> [String] -> State
gotoBrowserSuccess st ent =
  newst & footer .~ footers newst
    where
      newst = st & activeView .~ SearchView
                 & visibleEntries .~ toBrowserList ent
                 & allEntryNames .~ Map.singleton "." ent
                 & focusRing .~ F.focusRing [SearchField, BrowserField]

handleFieldInput :: State -> V.Event -> Maybe Field -> T.EventM Field State
handleFieldInput st e field =
  case field of
    Just PathField -> inner dbPathField
    Just PasswordField -> inner passwordField
    Just KeyfileField -> inner keyfileField
    _ -> pure st
  where
    inner :: Lens' State (E.Editor String Field) -> T.EventM Field State
    inner field_ = T.handleEventLensed st field_ E.handleEditorEvent e
