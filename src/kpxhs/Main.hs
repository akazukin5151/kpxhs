{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Brick.AttrMap      as A
import           Brick.BChan        (BChan, newBChan)
import qualified Brick.Focus        as F
import qualified Brick.Main         as M
import qualified Brick.Widgets.Edit as E
import           Control.Monad      (void, when)
import qualified Data.ByteString    as B
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Graphics.Vty       as V
import           System.Directory
    ( createDirectory
    , doesDirectoryExist
    , doesFileExist
    , getHomeDirectory
    )
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import Common   (annotate, defaultDialog, initialFooter, toBrowserList)
import Config   (parseConfig)
import Defaults (defaultConfigText, defaultThemeText, help)
import Events   (appEvent)
import Types
    ( Event
    , Field (KeyfileField, PasswordField, PathField, SearchField)
    , State (..)
    , View (LoginView)
    )
import UI       (drawUI)


initialState :: F.FocusRing Field -> Text -> Text -> Maybe Int -> BChan Event -> State
initialState ring dbdir kfdir timeout' chan =
  State
    { _visibleEntries     = toBrowserList [],
      _allEntryNames      = Map.empty,
      _selectedEntryName  = Nothing,
      _allEntryDetails    = Map.empty,
      _previousView       = LoginView,  -- doesn't really matter here
      _activeView         = LoginView,
      _footer             = annotate $ initialFooter ring,
      _focusRing          = ring,
      _dbPathField        = E.editor PathField (Just 1) dbdir,
      _passwordField      = E.editor PasswordField (Just 1) "",
      _keyfileField       = E.editor KeyfileField (Just 1) kfdir,
      _searchField        = E.editor SearchField (Just 1) "",
      _currentPath        = [],
      _exitDialog         = defaultDialog,
      _isClipboardCleared = True,
      _chan               = chan,
      _clearTimeout       = timeout',
      _countdownThreadId  = Nothing,
      _counterValue       = Nothing
    }

mkMap :: [(A.AttrName, V.Attr)] -> A.AttrMap
mkMap = A.attrMap V.defAttr

theApp :: [(A.AttrName, V.Attr)] -> M.App State Event Field
theApp theme =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = pure,
      M.appAttrMap = const $ mkMap theme
    }

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                           -> tui
    [x] | isCmd "help" x         -> putStrLn help
    [x] | isCmd "write-config" x -> writeConfig
    _                            -> putStrLn help >> exitFailure

tui :: IO ()
tui = do
  home <- getHomeDirectory
  let cfgdir = home ++ "/.config/kpxhs/"
  (timeout', dbdir, kfdir, ring, theme) <- parseConfig cfgdir

  chan <- newBChan 10
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  void $
    M.customMain initialVty buildVty (Just chan) (theApp theme)
      (initialState ring dbdir kfdir timeout' chan)

isCmd :: String -> String -> Bool
isCmd cmd string = s == pure (head cmd) || s == cmd
  where
    s = dropWhile (== '-') string

-- | Aborts if either config or theme exists, to prevent inconsistency
writeConfig :: IO ()
writeConfig = do
  home <- getHomeDirectory
  let cfgdir = home ++ "/.config/kpxhs/"
  dirExists <- doesDirectoryExist cfgdir
  when (not dirExists) $
    createDirectory cfgdir

  let cfgPath = cfgdir <> "config.hs"
  configExists <- doesFileExist cfgPath
  when configExists $
    putStrLn "config.hs already exists, aborting" >> exitFailure

  let themePath = cfgdir <> "theme.hs"
  themeExists <- doesFileExist themePath
  when themeExists $
     putStrLn "theme.hs already exists, aborting" >> exitFailure

  B.writeFile cfgPath $ encodeUtf8 defaultConfigText
  B.writeFile themePath $ encodeUtf8 defaultThemeText
