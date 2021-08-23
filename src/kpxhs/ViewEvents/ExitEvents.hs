module ViewEvents.ExitEvents (exitEvent) where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import           Brick.Widgets.Dialog   (handleDialogEvent)
import qualified Brick.Widgets.Dialog   as D
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Graphics.Vty           as V
import           Lens.Micro             ((&), (.~), (^.))
import           System.Info            (os)
import           System.Process         (callCommand)

import           Types                  ( ExitDialog (Cancel, Clear, Exit)
                                        , Field
                                        , State
                                        , activeView
                                        , exitDialog
                                        , previousView
                                        )


exitEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
exitEvent st (T.VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] -> handleEnter st
    _                   -> handleDialog st e
exitEvent st _ = M.continue st

-- | handleDialogEvent returns (EventM f (Dialog a)),
-- but (>>= M.continue) needs (EventM f State),
-- so a function is needed to transform
-- (EventM f (Dialog a)) to (EventM f State)
handleDialog :: State -> V.Event -> T.EventM Field (T.Next State)
handleDialog st e =
  handleDialogEvent e (st ^. exitDialog) >>= modifyDialog st >>= M.continue
    where
      modifyDialog :: State -> D.Dialog ExitDialog -> T.EventM Field State
      modifyDialog st' x = pure $ st' & exitDialog .~ x

handleEnter :: State -> T.EventM Field (T.Next State)
handleEnter st =
  case D.dialogSelection (st^.exitDialog) of
    Just Clear  -> liftIO clearClipboard >> M.halt st
    Just Cancel -> M.continue $ st & activeView .~ (st^.previousView)
    Just Exit   -> M.halt st
    _           -> M.continue st

clearClipboard :: IO ()
clearClipboard = callCommand $ "printf '' | " ++ handler where
  handler = case os of
    "linux" -> "xclip -selection clipboard"
    _       -> "pbcopy"
