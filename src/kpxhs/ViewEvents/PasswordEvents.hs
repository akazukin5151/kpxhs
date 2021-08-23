{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.PasswordEvents (passwordEvent) where

import qualified Brick.Focus            as F
import qualified Brick.Main             as M
import qualified Brick.Types            as T
import           Brick.Widgets.Core     (txt)
import qualified Brick.Widgets.Edit     as E
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as TT
import qualified Graphics.Vty           as V
import           Lens.Micro             (Lens', (%~), (&), (.~), (^.))
import           System.Exit            (ExitCode (ExitSuccess))

import           Common                 (footers, toBrowserList)
import           Types                  ( Action (Ls)
                                        , Field (..)
                                        , State
                                        , View (SearchView)
                                        , activeView
                                        , allEntryNames
                                        , dbPathField
                                        , focusRing
                                        , footer
                                        , keyfileField
                                        , passwordField
                                        , visibleEntries
                                        )
import           ViewEvents.Common      (getCreds, processInput, runCmd)


valid :: State -> Bool
valid st = f $ getCreds st
  where
    f (a, b, _) = not (TT.null a && TT.null b)

passwordEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
passwordEvent st (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc []              -> M.halt st
    V.EvKey (V.KChar '\t') []      -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab []          -> M.continue $ st & focusRing %~ F.focusPrev
    V.EvKey V.KEnter [] | valid st -> M.continue =<< liftIO (gotoBrowser st)
    _                              -> M.continue =<< handleFieldInput st e
passwordEvent st _ = M.continue st

gotoBrowser :: State -> IO State
gotoBrowser st = do
  let (dir, pw, kf) = getCreds st
  (code, stdout, stderr) <- runCmd Ls dir [] pw kf
  case code of
    ExitSuccess -> pure $ gotoBrowserSuccess st $ processInput stdout
    _           -> pure $ st & footer .~ txt stderr

gotoBrowserSuccess :: State -> [Text] -> State
gotoBrowserSuccess st ent =
  newst & footer .~ footers newst
    where
      newst = st & activeView .~ SearchView
                 & visibleEntries .~ toBrowserList ent
                 & allEntryNames .~ Map.singleton "." ent
                 & focusRing .~ F.focusRing [SearchField, BrowserField]

handleFieldInput :: State -> V.Event -> T.EventM Field State
handleFieldInput st e =
  case field of
    Just PathField     -> inner dbPathField
    Just PasswordField -> inner passwordField
    Just KeyfileField  -> inner keyfileField
    _                  -> pure st
  where
    field = F.focusGetCurrent (st ^. focusRing)
    inner :: Lens' State (E.Editor Text Field) -> T.EventM Field State
    inner field_ = T.handleEventLensed st field_ E.handleEditorEvent e
