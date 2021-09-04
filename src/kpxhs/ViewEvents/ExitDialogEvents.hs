module ViewEvents.ExitDialogEvents (exitEvent) where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import           Brick.Widgets.Dialog   (handleDialogEvent)
import qualified Brick.Widgets.Dialog   as D
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Functor           ((<&>))
import qualified Graphics.Vty           as V
import           Lens.Micro             ((&), (.~), (^.))

import Types
    ( ExitDialog (Cancel, Clear, Exit)
    , Field
    , State
    , activeView
    , exitDialog
    , previousView
    )
import ViewEvents.Common (updateFooter)
import ViewEvents.Copy   (clearClipboard)


exitEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
exitEvent st (T.VtyEvent (V.EvKey V.KEnter [])) = handleEnter st
exitEvent st (T.VtyEvent e)                     = handleDialog st e
exitEvent st _                                  = M.continue st

handleDialog :: State -> V.Event -> T.EventM Field (T.Next State)
handleDialog st e =
  handleDialogEventEsc e st >>= M.continue

handleDialogEventEsc :: V.Event -> State -> T.EventM n State
handleDialogEventEsc ev st =
  case ev of
    V.EvKey V.KEsc [] -> pure $ cancelExit st
    _                 -> handleDialogEvent ev (st^.exitDialog) <&> setDialog
  where
    -- handleDialogEvent returns EventM (Dialog a)
    -- setDialog transforms that inner Dialog back into a State
    setDialog :: D.Dialog ExitDialog -> State
    setDialog x = st & exitDialog .~ x

cancelExit :: State -> State
cancelExit st = st & activeView .~ (st^.previousView)
                   & updateFooter

handleEnter :: State -> T.EventM Field (T.Next State)
handleEnter st =
  case D.dialogSelection (st^.exitDialog) of
    Just Clear  -> liftIO clearClipboard >> M.halt st
    Just Cancel -> M.continue $ cancelExit st
    Just Exit   -> M.halt st
    _           -> M.continue st
