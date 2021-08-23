module ViewEvents.Common where

import qualified Brick.Focus            as F
import qualified Brick.Main             as M
import qualified Brick.Types            as T
import           Brick.Widgets.Core     (str)
import qualified Brick.Widgets.Edit     as E
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (isSuffixOf, partition, sort)
import qualified Data.Text.Zipper       as Z hiding (textZipper)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((%~), (&), (.~), (^.))
import           System.Exit            (ExitCode (ExitSuccess))
import           System.Process         (readProcessWithExitCode)

import           Common                 (footers)
import           Types                  ( Action (..)
                                        , CopyType (CopyUsername)
                                        , Field
                                        , State
                                        , View (BrowserView, ExitView, SearchView)
                                        , activeView
                                        , dbPathField
                                        , focusRing
                                        , footer
                                        , hasCopied
                                        , keyfileField
                                        , passwordField
                                        , previousView
                                        )


liftContinue :: (a -> b -> IO c) -> a -> b -> T.EventM n (T.Next c)
liftContinue g st x = liftIO (g st x) >>= M.continue

processInput :: String -> [String]
processInput s = dirs ++ entries_
  where
    (dirs, entries_) = partition ("/" `isSuffixOf`) $ sort $ lines s

prepareExit :: State -> State
prepareExit st =
  st & previousView .~ (st^.activeView)
     & activeView .~ ExitView

getCreds :: State -> (String, String, String)
getCreds st = (dir, pw, kf)
  where
    dir = extractTextField $ st ^. dbPathField
    pw = extractTextField $ st ^. passwordField
    kf = extractTextField $ st ^. keyfileField
    extractTextField :: E.Editor String Field -> String
    extractTextField field =
      let res = Z.getText $ field ^. E.editContentsL in
      case res of
        []      -> ""
        (x : _) -> x


runCmd :: Action
       -> String
       -> [String]
       -> String
       -> String
       -> IO (ExitCode, String, String)
runCmd Ls dir args pw kf   = _runCmdInner "ls" dir args pw kf
runCmd Clip dir args pw kf = _runCmdInner "clip" dir args pw kf
runCmd Show dir args pw kf = _runCmdInner "show" dir args pw kf

_runCmdInner :: String
            -> String
            -> [String]
            -> String
            -> String
            -> IO (ExitCode, String, String)
_runCmdInner action dir extraArgs pw kf =
  readProcessWithExitCode "keepassxc-cli" args pw
  where
    args = [action, dir, "--quiet"] ++ extraArgs_
    extraArgs_ = case kf of
                   "" -> extraArgs
                   _  -> ["-k", kf] ++ extraArgs

copyEntryCommon :: State -> String -> CopyType -> IO State
copyEntryCommon st entry ctype = do
  let (dir, pw, kf) = getCreds st
  let attr = _copyTypeToStr ctype
  (code, _, stderr) <- runCmd Clip dir [entry, "-a", attr] pw kf
  pure $ case code of
    ExitSuccess -> st
                   & footer .~ str (show attr ++ " copied to clipboard!")
                   & hasCopied .~ True
    _ -> st & footer .~ str stderr

_copyTypeToStr :: CopyType -> String
_copyTypeToStr ctype =
  case ctype of
    CopyUsername -> "username"
    _            -> "password"

commonTabEvent :: (State -> V.Event -> T.EventM Field (T.Next State))
               -> State
               -> T.BrickEvent Field e
               -> T.EventM Field (T.Next State)
commonTabEvent fallback st (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar '\t') [] -> f focusNext
    V.EvKey V.KBackTab []     -> f focusPrev
    _                         -> fallback st e
  where
    f x = M.continue $ _handleTab st x (st ^. activeView)
commonTabEvent _ st _ = M.continue st

_handleTab :: State -> (State -> View -> State) -> View -> State
_handleTab st f BrowserView = f st SearchView
_handleTab st f _           = f st BrowserView

focus :: (F.FocusRing Field -> F.FocusRing Field) -> State -> View -> State
focus f st view =
  newst & footer .~ footers newst
    where
      newst = st & focusRing %~ f
                 & activeView .~ view

focusNext :: State -> View -> State
focusNext = focus F.focusNext

focusPrev :: State -> View -> State
focusPrev = focus F.focusPrev
