module ViewEvents.ExitEvents (exitEvent) where

import Lens.Micro
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V
import Control.Monad.IO.Class
import System.Process
import System.Info

import Types


exitEvent :: State -> T.BrickEvent Field e -> T.EventM Field (T.Next State)
exitEvent st (T.VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] -> handleEnter st
    _ -> M.continue $ handleDialog st e
exitEvent st _ = M.continue st

handleEnter :: State -> T.EventM Field (T.Next State)
handleEnter st =
  case D.dialogSelection (st^.exitDialog) of
    Just Clear -> liftIO clearClipboard >> M.halt st
    Just Cancel -> M.continue $ st & activeView .~ (st^.previousView)
    _ -> M.halt st

clearClipboard :: IO ()
clearClipboard = callCommand $ "printf '' | " ++ handler where
  handler = case os of
    "linux" -> "xclip -selection clipboard"
    _ -> "pbcopy"

handleDialog :: State -> V.Event -> State
handleDialog st e = st & exitDialog %~ (handleDialogEvent' e)


-- Adapted from Brick.Widgets.Dialog
handleDialogEvent' :: V.Event -> D.Dialog a -> D.Dialog a
handleDialogEvent' ev d =
    case ev of
        V.EvKey (V.KChar '\t') [] -> nextButtonBy 1 True d
        V.EvKey V.KBackTab [] -> nextButtonBy (-1) True d
        V.EvKey V.KRight [] -> nextButtonBy 1 False d
        V.EvKey V.KLeft [] -> nextButtonBy (-1) False d
        _ -> d


-- Copied from Brick.Widgets.Dialog because it's private
-- The usage of mod is fine because there is always > 1 choices in dialog
nextButtonBy :: Int -> Bool -> D.Dialog a -> D.Dialog a
nextButtonBy amt wrapCycle d =
    let numButtons = length $ d^.D.dialogButtonsL
    in if numButtons == 0 then d
       else case d^.D.dialogSelectedIndexL of
           Nothing -> d & D.dialogSelectedIndexL ?~ 0
           Just i -> d & D.dialogSelectedIndexL ?~ newIndex
               where
                   addedIndex = i + amt
                   newIndex = if wrapCycle
                              then addedIndex `mod` numButtons
                              else max 0 $ min addedIndex $ numButtons - 1
