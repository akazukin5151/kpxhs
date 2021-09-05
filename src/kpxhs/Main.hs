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
import           System.FilePath    (takeFileName, (</>))

import Common          (annotate, defaultDialog, initialFooter, toBrowserList)
import Config.Config   (parseConfig)
import Config.Defaults (defaultConfigText, defaultThemeText)
import Constants       (help, version)
import Events          (appEvent)
import Types
    ( Event
    , Field (KeyfileField, PasswordField, PathField, SearchField)
    , State (..)
    , View (LoginView)
    )
import UI              (drawUI)


initialState :: F.FocusRing Field
             -> Text -> Text -> Maybe Int -> BChan Event -> A.AttrMap -> State
initialState ring dbdir kfdir timeout' chan theMap =
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
      _counterValue       = Nothing,
      _currentCmd         = "",
      _theMap             = theMap
    }

mkMap :: [(A.AttrName, V.Attr)] -> A.AttrMap
mkMap = A.attrMap V.defAttr

theApp :: A.AttrMap -> M.App State Event Field
theApp theMap =
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
    []                           -> tui
    [x] | isCmd "version" x      -> print version
    [x] | isCmd "help" x         -> putStrLn help
    [x] | isCmd "write-config" x -> writeConfig
    _                            -> putStrLn help >> exitFailure

tui :: IO ()
tui = do
  home <- getHomeDirectory
  let cfgdir = home </> ".config/kpxhs/"
  (timeout', dbdir, kfdir, ring, theme) <- parseConfig cfgdir
  let theMap = mkMap theme

  chan <- newBChan 10
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  void $
    M.customMain initialVty buildVty (Just chan) (theApp theMap)
      (initialState ring dbdir kfdir timeout' chan theMap)

-- `head` is safe because the cmd is hardcoded by me,
-- not passed in by the user
isCmd :: String -> String -> Bool
isCmd cmd string = s == pure (head cmd) || s == cmd
  where
    s = dropWhile (== '-') string

-- | Aborts if either config or theme exists, before writing, to prevent inconsistency
-- Logs the write because if one failed and the other succeeded for some reason,
-- the user should be warned that the dir is potentially incomplete/inconsistent
writeConfig :: IO ()
writeConfig = do
  home <- getHomeDirectory
  let cfgdir = home </> ".config/kpxhs/"
  dirExists <- doesDirectoryExist cfgdir

  let cfgPath = cfgdir </> "config.hs"
  let themePath = cfgdir </> "theme.hs"
  if dirExists
     then assertFileDoesntExist cfgPath >> assertFileDoesntExist themePath
     else createDirectory cfgdir

  B.writeFile cfgPath $ encodeUtf8 defaultConfigText
  putStrLn $ "Config written to " <> cfgPath
  B.writeFile themePath $ encodeUtf8 defaultThemeText
  putStrLn $ "Theme written to " <> themePath

assertFileDoesntExist :: FilePath -> IO ()
assertFileDoesntExist path = do
  exists <- doesFileExist path
  when exists $
    putStrLn (takeFileName path <> " already exists, aborting") >> exitFailure
