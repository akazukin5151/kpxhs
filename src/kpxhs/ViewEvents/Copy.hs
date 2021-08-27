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
    ( Action (Clip)
    , CopyType (CopyUsername)
    , Event (ClearClipCount, Copying)
    , State
    , View (BrowserView, SearchView)
    , activeView
    , chan
    , clearTimeout
    , countdownThreadId
    , currentCountdown
    , footer
    , hasCopied
    )
import ViewEvents.Utils (getCreds, runCmd)


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
handleCopy st (ExitFailure _, stderr) = pure $ st & footer .~ txt stderr
handleCopy st (ExitSuccess, _)        =
  case st ^. clearTimeout of
    Nothing       -> pure $ st & footer .~ txt "Copied!"
    Just timeout' -> handleCopyInner st timeout'

handleCopyInner :: State -> Int -> IO State
handleCopyInner st timeout' = do
  -- If there already exists a countdown thread, kill it first to prevent interference
  case st ^. countdownThreadId of
    Just x  -> killThread x
    Nothing -> pure ()
  -- Save the tid in case if it needs to be cancelled later
  tid <- forkIO $ writeBChan (st^.chan) $ ClearClipCount timeout'
  pure $ st & hasCopied .~ True
            & countdownThreadId ?~ tid

handleClipCount :: State -> Int -> IO State
handleClipCount st 0     =
  clearClipboard >> pure (st & footer .~ txt "Clipboard cleared"
                             & hasCopied .~ False
                             & countdownThreadId .~ Nothing)
handleClipCount st count =
  case st ^. clearTimeout of
    Nothing       -> pure st
    Just timeout' -> handleClipCountInner st count timeout'

handleClipCountInner :: State -> Int -> Int -> IO State
handleClipCountInner st count timeout' = do
  -- Even if the footer shouldn't be changed, the countdown should proceed
  let changeFooter = case st^.activeView of
                       BrowserView -> True
                       SearchView  -> True
                       _           -> False
  let bg_cmd = threadDelay 1000000
              >> writeBChan (st^.chan) (ClearClipCount (count - 1))
  -- Save the tid in case if it needs to be cancelled later
  tid <- forkIO bg_cmd
  let label = mkCountdownLabel count
  -- https://github.com/NorfairKing/haskell-dangerous-functions#fromintegral
  -- I think Int -> Float is fine because Float is larger than Int
  -- so an int shouldn't be truncated
  let v = fromIntegral count / fromIntegral timeout'
  let f = if changeFooter
             then footer .~ P.progressBar label v
             else id
  pure $ st & f
            & countdownThreadId ?~ tid
            & currentCountdown ?~ v

mkCountdownLabel :: Show a => a -> Maybe String
mkCountdownLabel count =
  Just $ "Clearing clipboard in " <> show count <> " seconds"

clearClipboard :: IO ()
clearClipboard = callCommand $ "printf '' | " ++ handler where
  handler = case os of
    "linux" -> "xclip -selection clipboard"
    _       -> "pbcopy"
