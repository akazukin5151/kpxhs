{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.LoginFrozenEvents (loginFrozenEvent) where

import qualified Brick.Main         as M
import qualified Brick.Types        as T
import           Brick.Widgets.Core (txt)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import           Lens.Micro         ((&), (.~), (^.))
import           System.Exit        (ExitCode (ExitSuccess))

import Common            (toBrowserList)
import Types
    ( Event (Login)
    , Field (SearchField)
    , State
    , activeView
    , allEntryNames
    , footer
    , visibleEntries
    , View (BrowserView, SearchView, LoginView), fieldToFocus
    )
import ViewEvents.Common (updateFooter)
import ViewEvents.Utils  (processStdout)

loginFrozenEvent :: State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State)
loginFrozenEvent st (T.AppEvent e) = M.continue $ gotoBrowser st e
loginFrozenEvent st _              = M.continue st

gotoBrowser :: State -> Event -> State
gotoBrowser st (Login (ExitSuccess, stdout, _)) = gotoBrowserSuccess st
                                                    $ processStdout stdout
gotoBrowser st (Login (_, _, stderr))           = loginFail st stderr
gotoBrowser st _                                = st

gotoBrowserSuccess :: State -> [Text] -> State
gotoBrowserSuccess st ent =
  st & activeView     .~ view
     & visibleEntries .~ toBrowserList ent
     & allEntryNames  .~ Map.singleton "." ent
     & updateFooter
  where
    field = st ^. fieldToFocus
    view = if field == SearchField then SearchView else BrowserView

loginFail :: State -> Text -> State
loginFail st stderr =
  st & footer .~ txt stderr
     & activeView .~ LoginView
