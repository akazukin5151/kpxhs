module ViewEvents.BrowserEvents.VimCommand (handleVimDigit, handleVimMotion) where

import Brick         (Direction (Down, Up))
import Data.Function ((&))
import Lens.Micro    ((%~), (.~), (^.))
import Text.Read     (readMaybe)

import Types                          (State, currentCmd, visibleEntries)
import ViewEvents.BrowserEvents.Utils (listMoveWith)
import ViewEvents.Common              (updateFooterGuarded)

handleVimDigit :: State -> Char -> State
handleVimDigit st x =
  st & currentCmd %~ (<> [x])

toDirection :: Char -> Maybe Direction
toDirection 's' = Just Down
toDirection 'j' = Just Down
toDirection 'w' = Just Up
toDirection 'k' = Just Up
toDirection _   = Nothing

handleVimMotion :: State -> Char -> State
handleVimMotion st c =
  case (mnum, msign) of
    (Just num, Just sign) -> st & currentCmd .~ ""
                                & visibleEntries %~ listMoveWith (sign num)
                                & updateFooterGuarded
    _ -> st
  where
    mnum = case st ^. currentCmd of
            [] -> Nothing
            xs -> readMaybe xs
    msign = case toDirection c of
             Just Down -> Just (+)
             Just Up   -> Just subtract
             Nothing   -> Nothing
