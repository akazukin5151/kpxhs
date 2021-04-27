{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import System.Environment
import System.Directory
import Control.Exception
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as V
import Brick.Util (fg, on)
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L

import Common
import Events
import Types
import UI


initialState :: F.FocusRing Field -> String -> String -> State
initialState ring dbdir kfdir =
  State
    { _visibleEntries = toBrowserList [],
      _allEntryNames = Map.empty,
      _currentEntryDetailName = Nothing,
      _allEntryDetails = Map.empty,
      _activeView = PasswordView,
      _footer = "", -- doesn't matter here
      _focusRing = ring,
      _dbPathField = E.editor PathField (Just 1) dbdir,
      _passwordField = E.editor PasswordField (Just 1) "",
      _keyfileField = E.editor KeyfileField (Just 1) kfdir,
      _searchField = E.editor SearchField (Just 1) "",
      _currentDir = []
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, fg V.red),
      (L.listSelectedAttr <> "custom", fg V.cyan),
      (E.editAttr, V.black `on` V.white),
      (E.editFocusedAttr, V.white `on` V.blue)
    ]

theApp :: M.App State e Field
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = pure,
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> tui
    [x] | isHelp x -> showHelp
    _ -> showHelp >> exitWith (ExitFailure 1)

tui :: IO ()
tui = do
  home <- getHomeDirectory
  let cfgdir = home ++ "/.config/kpxhs/config"
  (dbdir, kfdir, ring) <- parseConfig cfgdir
  void $ M.defaultMain theApp (initialState ring dbdir kfdir)

parseConfig :: String -> IO (String, String, F.FocusRing Field)
parseConfig cfgdir = do
  file <- catch (readFile cfgdir) f
  case lines file of
    [dbdir, kfdir] | dbdir /= "" && kfdir /= "" ->
      pure (dbdir, kfdir, passwordfirst)
    [dbdir] | dbdir /= "" ->
      pure (dbdir, "", passwordfirst)
    _ ->
      pure ("", "", pathfirst)
  where
    f :: IOException -> IO String
    f _ = pure ""
    pathfirst = F.focusRing [PathField, PasswordField, KeyfileField]
    passwordfirst = F.focusRing [PasswordField, KeyfileField, PathField]

isHelp :: String -> Bool
isHelp str = s == "h" || s == "help"
  where
    s = dropWhile (== '-') str

showHelp :: IO ()
showHelp =
  putStrLn "kpxhs - Interactive Keepass database TUI viewer\n\
            \  Usage\n\
            \    kpxhs                  Start the program\n\
            \    kpxhs [-h | --help]    Show this help\n\n\
            \  TUI keybindings (in general)\n\
            \    Esc                    Quit\n\
            \    Tab                    Cycle focus\n\
            \    Enter                  Show entry details\n\
            \    u                      Copy username\n\
            \    p                      Copy password"

