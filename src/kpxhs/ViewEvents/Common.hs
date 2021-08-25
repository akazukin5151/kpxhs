{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.Common where

import           Brick.BChan               (writeBChan)
import qualified Brick.Focus               as F
import qualified Brick.Main                as M
import           Brick.Types               (Widget)
import qualified Brick.Types               as T
import           Brick.Widgets.Core        (txt)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import qualified Brick.Widgets.ProgressBar as P
import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (liftIO)
import           Data.List                 (partition, sort)
import           Data.Maybe                (fromMaybe, isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as TT
import qualified Data.Text.Zipper          as Z hiding (textZipper)
import           GHC.Conc                  (killThread)
import qualified Graphics.Vty              as V
import           Lens.Micro                ((%~), (&), (.~), (?~), (^.))
import           System.Exit               (ExitCode (ExitSuccess))
import           System.Info               (os)
import           System.Process
    ( callCommand
    , readProcessWithExitCode
    )

import Common (annotate, exit, initialFooter, tab)
import Types
    ( Action (Clip, Ls, Show)
    , CmdOutput
    , CopyType (CopyUsername)
    , Event (ClearClipCount, Copying)
    , Field
    , State
    , View (BrowserView, EntryView, ExitView, PasswordView, SearchView)
    , activeView
    , chan
    , clearTimeout
    , countdownThreadId
    , currentDir
    , dbPathField
    , focusRing
    , footer
    , hasCopied
    , keyfileField
    , passwordField
    , previousView
    , visibleEntries
    )


liftContinue1 :: (a -> IO b) -> a -> T.EventM n (T.Next b)
liftContinue1 g st = liftIO (g st) >>= M.continue

liftContinue2 :: (a -> b -> IO c) -> a -> b -> T.EventM n (T.Next c)
liftContinue2 g st x = liftIO (g st x) >>= M.continue

processInput :: Text -> [Text]
processInput s = dirs ++ entries_
  where
    (dirs, entries_) = partition ("/" `TT.isSuffixOf`) x
    x = sort $ TT.lines s

prepareExit :: State -> State
prepareExit st =
  st & previousView .~ (st^.activeView)
     & activeView .~ ExitView

getCreds :: State -> (Text, Text, Text)
getCreds st = (dir, pw, kf)
  where
    dir = extractTextField $ st ^. dbPathField
    pw = extractTextField  $ st ^. passwordField
    kf = extractTextField  $ st ^. keyfileField
    extractTextField :: E.Editor Text Field -> Text
    extractTextField field =
      let res = Z.getText $ field ^. E.editContentsL in
      case res of
        []      -> ""
        (x : _) -> x


runCmd :: Action
       -> Text
       -> [Text]
       -> Text
       -> Text
       -> IO CmdOutput
runCmd Ls dir args pw kf   = _runCmdInner "ls" dir args pw kf
runCmd Clip dir args pw kf = _runCmdInner "clip" dir args pw kf
runCmd Show dir args pw kf = _runCmdInner "show" dir args pw kf

_runCmdInner :: Text
             -> Text
             -> [Text]
             -> Text
             -> Text
             -> IO CmdOutput
_runCmdInner action dir extraArgs pw kf = do
  (e, a, b) <- readProcessWithExitCode "keepassxc-cli" args (TT.unpack pw)
  pure (e, TT.pack a, TT.pack b)
  where
    args :: [String]
    args = TT.unpack <$> [action, dir] ++ extraArgs_
    extraArgs_ = case kf of
                   "" -> extraArgs
                   _  -> ["-k", kf] ++ extraArgs

_copyTypeToStr :: CopyType -> Text
_copyTypeToStr CopyUsername = "username"
_copyTypeToStr _            = "password"

copyEntryCommon :: State -> Text -> CopyType -> IO State
copyEntryCommon st entry ctype = do
  let (dir, pw, kf) = getCreds st
  let attr = _copyTypeToStr ctype
  let bg_cmd = do
        (code, _, stderr) <- runCmd Clip dir [entry, "-a", attr] pw kf
        writeBChan (st^.chan) $ Copying (code, stderr)
  void $ forkIO bg_cmd
  pure $ st & footer .~ txt "Copying..."

handleCopy :: State -> (ExitCode, Text) -> IO State
handleCopy st (ExitSuccess, _) = do
  -- If there already exists a countdown thread, kill it first to prevent interference
  case st ^. countdownThreadId of
    Just x  -> killThread x
    Nothing -> pure ()
  -- Save the tid in case if it needs to be cancelled later
  tid <- forkIO $ writeBChan (st^.chan) $ ClearClipCount (st^.clearTimeout)
  pure $ st & hasCopied .~ True
            & countdownThreadId ?~ tid
handleCopy st (_, stderr)      = pure $ st & footer .~ txt stderr

handleClipCount :: State -> Int -> IO State
handleClipCount st 0     =
  clearClipboard >> pure (st & footer .~ txt "Clipboard cleared"
                             & hasCopied .~ False
                             & countdownThreadId .~ Nothing)
handleClipCount st count = do
  let bg_cmd = threadDelay 1000000
              >> writeBChan (st^.chan) (ClearClipCount (count - 1))
  -- Save the tid in case if it needs to be cancelled later
  tid <- forkIO bg_cmd
  let label = Just $ "Clearing clipboard in " <> show count <> " seconds"
  let v = fromIntegral count / fromIntegral (st ^. clearTimeout)
  pure $ st & footer .~ P.progressBar label v
            & countdownThreadId ?~ tid

clearClipboard :: IO ()
clearClipboard = callCommand $ "printf '' | " ++ handler where
  handler = case os of
    "linux" -> "xclip -selection clipboard"
    _       -> "pbcopy"

commonTabEvent :: (State -> T.BrickEvent Field Event -> T.EventM Field (T.Next State))
               -> State
               -> T.BrickEvent Field Event
               -> T.EventM Field (T.Next State)
commonTabEvent fallback st e =
  case e of
    T.VtyEvent (V.EvKey (V.KChar '\t') []) -> f focusNext
    T.VtyEvent (V.EvKey V.KBackTab [])     -> f focusPrev
    T.AppEvent (ClearClipCount count)      -> liftContinue2 handleClipCount st count
    _                                      -> fallback st e
  where
    f x = M.continue $ _handleTab st x (st ^. activeView)

_handleTab :: State -> (State -> View -> State) -> View -> State
_handleTab st f BrowserView = f st SearchView
_handleTab st f _           = f st BrowserView

focus :: (F.FocusRing Field -> F.FocusRing Field) -> State -> View -> State
focus f st view =
  st & focusRing %~ f
     & activeView .~ view
     & updateFooterGuarded

focusNext :: State -> View -> State
focusNext = focus F.focusNext

focusPrev :: State -> View -> State
focusPrev = focus F.focusPrev

-- | Restores the default footer for the current view
--  Should be only used when transitioning to a new view or field
updateFooter :: State -> State
updateFooter st = st & footer .~ viewDefaultFooter st

updateFooterGuarded :: State -> State
updateFooterGuarded st =
  if isJust $ st ^. countdownThreadId
     then st
     else st & updateFooter

viewDefaultFooter :: State -> Widget Field
viewDefaultFooter st =
  annotate $ case st^.activeView of
    SearchView   -> [exit, tab " focus list "]
    EntryView    -> [back, username, password]
    PasswordView -> initialFooter $ st ^. focusRing
    ExitView     -> [("", "")]
    BrowserView ->
      let extra = if isCopyable st then [username, password] else [] in
      case st^.currentDir of
        [] -> [exit, focus_search] <> extra
        _  -> [back, focus_search] <> extra
  where
    back = ("Esc", " back  ")
    username = ("u", " copy username  ")
    password = ("p", " copy password")

focus_search :: (Text, Text)
focus_search = ("Tab", " focus search  ")

isDir :: State -> Bool
isDir st = fromMaybe False (processSelected f st)
  where
    f entry = TT.last entry == '/'

isGoUpToParent :: State -> Bool
isGoUpToParent st = fromMaybe False (processSelected f st)
  where
    f entry = entry == "-- (Go up parent) --"

isCopyable :: State -> Bool
isCopyable st = not (isDir st || isGoUpToParent st)

processSelected :: (Text -> a) -> State -> Maybe a
processSelected f st = do
  (_, entry) <- L.listSelectedElement $ st ^. visibleEntries
  pure $ f entry
