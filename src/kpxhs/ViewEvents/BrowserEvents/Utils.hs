-- | Utils to navigate around the list
{-# LANGUAGE OverloadedStrings #-}

module ViewEvents.BrowserEvents.Utils
    ( listMoveWith
    , listMovePageUp
    , listMovePageDown) where

import           Brick.Util         (clamp)
import qualified Brick.Widgets.List as L
import           Data.Maybe         (fromMaybe)

listMoveWith :: (L.Splittable t, Foldable t)
           => (Int -> Int) -> L.GenericList n t a -> L.GenericList n t a
listMoveWith f l = L.listMoveTo clamped l
  where
    clamped = clamp 0 (length $ L.listElements l) num
    num = f (fromMaybe 0 $ L.listSelected l)

-- Default page up and down functions too fast for me
listMovePageUp :: (L.Splittable t, Foldable t)
               => L.GenericList n t a -> L.GenericList n t a
listMovePageUp = listMoveWith (subtract 5)

listMovePageDown :: (L.Splittable t, Foldable t)
                 => L.GenericList n t a -> L.GenericList n t a
listMovePageDown = listMoveWith (5 +)
