-- Handles all copying and clipboard matters
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.Copy where

import           Brick.BChan               (writeBChan)
import           Brick.Widgets.Core        (txt)
import qualified Brick.Widgets.ProgressBar as P
import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Monad             (void)
import           Data.Function             ((&))
import           Data.Text                 (Text)
import           GHC.Conc                  (killThread)
import           GHC.IO.Exception          (ExitCode (ExitSuccess))
import           Lens.Micro                ((.~), (?~), (^.))
import           System.Exit               (ExitCode (ExitFailure))
import           System.Info               (os)
import           System.Process            (callCommand)

import Types
    ( CmdAction (Clip)
    , CopyType (CopyUsername)
    , Event (ClearClipCount, Copying)
    , State
    , View (BrowserView, EntryDetailsView, SearchView)
    , activeView
    , chan
    , clearTimeout
    , countdownThreadId
    , counterValue
    , footer
    , isClipboardCleared
    )
import ViewEvents.Utils (getCreds, runCmd)
import Data.Functor (($>))
import Data.Foldable (forM_)


_copyTypeToStr :: CopyType -> Text
_copyTypeToStr CopyUsername = "username"
_copyTypeToStr _            = "password"

copyEntryCommon :: State -> Text -> CopyType -> IO State
copyEntryCommon st entry ctype = do
  let (dir, pw, kf) = getCreds st
  let attr = _copyTypeToStr ctype
  void $ forkIO $ do
    (code, _, stderr) <- runCmd Clip dir [entry, "-a", attr, "0"] pw kf
    writeBChan (st^.chan) $ Copying (code, stderr)
  pure $ st & footer .~ txt "Copying..."

handleCopy :: State -> (ExitCode, Text) -> IO State
handleCopy st (ExitFailure _, stderr) = pure $ st & footer .~ txt stderr
handleCopy st (ExitSuccess, _)        =
  case st ^. clearTimeout of
    Nothing       -> pure $ st & footer .~ txt "Copied!"
    Just timeout' -> handleCopyInner st timeout'

handleCopyInner :: State -> Int -> IO State
handleCopyInner st timeout' = do
  -- If there already exists a countdown thread, kill it first to
  -- prevent interference
  forM_ (st ^. countdownThreadId) killThread
  -- Save the tid in case if it needs to be cancelled later
  tid <- forkIO $ writeBChan (st^.chan) $ ClearClipCount timeout'
  pure $ st & isClipboardCleared .~ False
            & countdownThreadId  ?~ tid

handleClipCount :: State -> Int -> IO State
handleClipCount st 0     =
  clearClipboard $>
    (st & footer             .~ txt "Clipboard cleared"
        & isClipboardCleared .~ True
        & countdownThreadId  .~ Nothing
        & counterValue       .~ Nothing)
handleClipCount st count =
  case st ^. clearTimeout of
    Nothing       -> pure st
    Just timeout' -> handleClipCountInner st count timeout'

-- Even if the footer shouldn't be changed (eg, in another view),
-- the countdown should proceed, because if the view is updated to something
-- where the footer should change, then the progress bar should appear again
handleClipCountInner :: State -> Int -> Int -> IO State
handleClipCountInner st count timeout' = do
  -- Save the tid in case if it needs to be cancelled later
  tid <- forkIO $ do
    threadDelay 1000000
    writeBChan (st^.chan) (ClearClipCount (count - 1))
  pure $ st & f
            & countdownThreadId ?~ tid
            & counterValue      ?~ v
  where
    label = mkCountdownLabel count
    -- https://github.com/NorfairKing/haskell-dangerous-functions#fromintegral
    -- I think Int -> Float is fine because Float is larger than Int
    -- so an int shouldn't be truncated
    v = fromIntegral count / fromIntegral timeout'
    f = if st^.activeView `elem` [BrowserView, SearchView, EntryDetailsView]
               then footer .~ P.progressBar label v
               else id

mkCountdownLabel :: Show a => a -> Maybe String
mkCountdownLabel count =
  Just $ "Clearing clipboard in " <> show count <> " seconds"

clearClipboard :: IO ()
clearClipboard = callCommand $ "printf '' | " ++ handler
  where
    handler = case os of
      "linux" -> "xclip -selection clipboard"
      _       -> "pbcopy"
