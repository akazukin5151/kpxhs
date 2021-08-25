{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Brick.AttrMap             as A
import           Brick.BChan               (BChan, newBChan)
import qualified Brick.Focus               as F
import qualified Brick.Main                as M
import           Brick.Util                (bg, fg, on)
import qualified Brick.Widgets.Dialog      as D
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import qualified Brick.Widgets.ProgressBar as P
import           Control.Monad             (void)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Graphics.Vty              as V
import           System.Directory          (getHomeDirectory)
import           System.Environment        (getArgs)
import           System.Exit               (ExitCode (ExitFailure), exitWith)

import Common (annotate, initialFooter, toBrowserList)
import Config (parseConfig)
import Events (appEvent)
import Types
    ( Event
    , ExitDialog (Cancel, Clear, Exit)
    , Field (KeyfileField, PasswordField, PathField, SearchField)
    , State (..)
    , View (PasswordView)
    )
import UI     (drawUI)


initialState :: F.FocusRing Field -> Text -> Text -> Int -> BChan Event -> State
initialState ring dbdir kfdir timeout' chan =
  State
    { _visibleEntries = toBrowserList [],
      _allEntryNames = Map.empty,
      _currentEntryDetailName = Nothing,
      _allEntryDetails = Map.empty,
      _previousView = PasswordView,  -- doesn't really matter here
      _activeView = PasswordView,
      _footer = annotate $ initialFooter ring,
      _focusRing = ring,
      _dbPathField = E.editor PathField (Just 1) dbdir,
      _passwordField = E.editor PasswordField (Just 1) "",
      _keyfileField = E.editor KeyfileField (Just 1) kfdir,
      _searchField = E.editor SearchField (Just 1) "",
      _currentDir = [],
      _exitDialog = D.dialog Nothing (Just (0, choices)) 60,
      _hasCopied = False,
      _chan = chan,
      _clearTimeout = timeout',
      _countdownThreadId = Nothing
    }
      where
        choices = [ ("Clear and exit", Clear)
                  , ("Just exit", Exit)
                  , ("Do not exit", Cancel)
                  ]

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr,             fg V.red),
      (L.listSelectedAttr <> "custom", fg V.cyan),
      (E.editAttr,                     V.black `on` V.white),
      (E.editFocusedAttr,              V.white `on` V.blue),
      (D.dialogAttr,                   V.white `on` V.blue),
      (D.buttonAttr,                   V.black `on` V.white),
      (D.buttonSelectedAttr,           bg V.yellow),
      ("key",                          bg V.white),
      ("label",                        fg V.black),
      (P.progressCompleteAttr,         V.white `on` V.blue)
    ]

theApp :: M.App State Event Field
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
    []             -> tui
    [x] | isHelp x -> showHelp
    _              -> showHelp >> exitWith (ExitFailure 1)

tui :: IO ()
tui = do
  home <- getHomeDirectory
  let cfgdir = home ++ "/.config/kpxhs/config"
  (timeout', dbdir, kfdir, ring) <- parseConfig cfgdir

  chan <- newBChan 10
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  void $
    M.customMain initialVty buildVty (Just chan) theApp
      (initialState ring dbdir kfdir timeout' chan)

isHelp :: String -> Bool
isHelp string = s == "h" || s == "help"
  where
    s = dropWhile (== '-') string

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
            \    p                      Copy password\n\
            \  Navigation\n\
            \    j, s                   Move down\n\
            \    k, w                   Move up\n\
            \    g                      Move to top\n\
            \    G                      Move to bottom\n\
            \    q                      Page up\n\
            \    e                      Page down"
