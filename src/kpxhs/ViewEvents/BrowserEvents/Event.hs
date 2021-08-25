-- | Functions that handles custom AppEvents (that the browser should handle),
-- such as the completion of a shell command, or the copy countdown
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.BrowserEvents.Event (handleAppEvent) where

import Brick.Widgets.Core (txt)
import Data.Text          (Text)
import GHC.IO.Exception   (ExitCode (ExitSuccess))
import Lens.Micro         ((&), (.~))

import Types
    ( CmdOutput
    , Event (Copying, EnterDir, ShowEntry)
    , State
    , footer
    )
import ViewEvents.BrowserEvents.Utils (enterDirSuccess, showEntrySuccess)
import ViewEvents.Copy                (handleCopy)
import ViewEvents.Utils               (processStdout)

handleAppEvent :: State -> Event -> IO State
handleAppEvent st (ShowEntry entry out)  = pure $ handleShowEntryEvent st entry out
handleAppEvent st (EnterDir entry out)   = pure $ handleEnterDirEvent st entry out
handleAppEvent st (Copying e)            = handleCopy st e
handleAppEvent st _                      = pure st

handleEnterDirEvent :: State -> Text -> CmdOutput -> State
handleEnterDirEvent st entry (ExitSuccess, stdout, _) =
  enterDirSuccess st ("-- (Go up parent) --" : processStdout stdout) entry
handleEnterDirEvent st _ (_, _, stderr) =
  st & footer .~ txt stderr

handleShowEntryEvent :: State -> Text -> CmdOutput -> State
handleShowEntryEvent st entry (ExitSuccess, stdout, _) = showEntrySuccess st entry stdout
handleShowEntryEvent st _     (_, _, stderr)           = st & footer .~ txt stderr
