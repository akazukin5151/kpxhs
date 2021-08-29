{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module ViewEvents.LoginEvents (passwordEvent) where

import           Brick.BChan        (writeBChan)
import qualified Brick.Focus        as F
import qualified Brick.Main         as M
import qualified Brick.Types        as T
import           Brick.Widgets.Core (txt)
import qualified Brick.Widgets.Edit as E
import           Control.Concurrent (forkIO)
import           Control.Monad      (void)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import qualified Data.Text          as TT
import qualified Graphics.Vty       as V
import           Lens.Micro         (Lens', (%~), (&), (.~), (^.))
import           System.Exit        (ExitCode (ExitSuccess))

import Common            (toBrowserList)
import Types
    ( CmdAction (Ls)
    , Event (Login)
    , Field (BrowserField, KeyfileField, PasswordField, PathField, SearchField)
    , State
    , View (SearchView)
    , activeView
    , allEntryNames
    , chan
    , dbPathField
    , focusRing
    , footer
    , keyfileField
    , passwordField
    , visibleEntries
    )
import ViewEvents.Common (liftContinue1, updateFooter)
import ViewEvents.Utils  (getCreds, processStdout, runCmd)


valid :: State -> Bool
valid st = f $ getCreds st
  where
    f (a, b, _) = not (TT.null a && TT.null b)

passwordEvent :: State -> T.BrickEvent n Event -> T.EventM Field (T.Next State)
passwordEvent st (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc []              -> M.halt st
    V.EvKey (V.KChar '\t') []      -> focus F.focusNext st
    V.EvKey V.KBackTab []          -> focus F.focusPrev st
    V.EvKey V.KEnter [] | valid st -> liftContinue1 loginInBackground st
    _                              -> M.continue =<< handleFieldInput st e
passwordEvent st (T.AppEvent e) = M.continue $ gotoBrowser st e
passwordEvent st _ = M.continue st

focus :: (F.FocusRing Field -> F.FocusRing Field)
      -> State
      -> T.EventM Field (T.Next State)
focus f st = M.continue $ st & focusRing %~ f
                             & updateFooter

loginInBackground :: State -> IO State
loginInBackground st = do
  let (dir, pw, kf) = getCreds st
  void $ forkIO $ do
      (code, stdout, stderr) <- runCmd Ls dir [] pw kf
      writeBChan (st^.chan) $ Login (code, stdout, stderr)
  pure $ st & footer .~ txt "Logging in..."

gotoBrowser :: State -> Event -> State
gotoBrowser st (Login (ExitSuccess, stdout, _)) = gotoBrowserSuccess st
                                                    $ processStdout stdout
gotoBrowser st (Login (_, _, stderr))           = st & footer .~ txt stderr
gotoBrowser st _                                = st

gotoBrowserSuccess :: State -> [Text] -> State
gotoBrowserSuccess st ent =
  st & activeView     .~ SearchView
     & visibleEntries .~ toBrowserList ent
     & allEntryNames  .~ Map.singleton "." ent
     & focusRing      .~ F.focusRing [SearchField, BrowserField]
     & updateFooter

handleFieldInput :: State -> V.Event -> T.EventM Field State
handleFieldInput st e =
  case F.focusGetCurrent (st ^. focusRing) of
    Just PathField     -> inner dbPathField
    Just PasswordField -> inner passwordField
    Just KeyfileField  -> inner keyfileField
    _                  -> pure st
  where
    inner :: Lens' State (E.Editor Text Field) -> T.EventM Field State
    inner field_ = T.handleEventLensed st field_ E.handleEditorEvent e
