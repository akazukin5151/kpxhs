{-# LANGUAGE OverloadedStrings #-}

module UI.BrowserUI (drawBrowser) where

import qualified Brick.AttrMap        as A
import qualified Brick.Focus          as F
import           Brick.Types          (Widget)
import           Brick.Util           (fg)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
    ( hLimitPercent
    , str
    , txt
    , updateAttrMap
    , vBox
    , vLimitPercent
    , (<+>)
    )
import qualified Brick.Widgets.List   as L
import           Data.Text            (pack)
import qualified Data.Text            as TT
import qualified Data.Vector          as Vec
import qualified Graphics.Vty         as V
import           Lens.Micro           ((&), (^.))

import Common    (pathToStr)
import Types
    ( Field (BrowserField)
    , State
    , currentPath
    , focusRing
    , footer
    , searchField
    , visibleEntries
    )
import UI.Common (getEditor)


drawBrowser :: State -> [Widget Field]
drawBrowser st = [ui]
  where
    ui =
      C.vCenter $
        vBox
          [  C.hCenter $ drawSearchBox st,
             C.hCenter $ drawBrowserList st,
             C.hCenter $ st ^. footer
          ]

drawSearchBox :: State -> Widget Field
drawSearchBox st = str "Search: " <+> hLimitPercent 75 ed
  where
    ed = getEditor st searchField TT.unlines

drawBrowserList :: State -> Widget Field
drawBrowserList st =
  st & drawBrowserListInner
     & vLimitPercent 90
     & hLimitPercent 90
     & drawBrowserLabel st
     & drawBorderColor st

drawBrowserLabel :: State -> Widget Field -> Widget Field
drawBrowserLabel st = B.borderWithLabel label
  where
    label = foldr1 (<+>) $ txt <$> [currentPath_, " ", "(", cur, "/", total, ")"]
    currentPath_ =
      case pathToStr $ st^.currentPath of
        "" -> "(Root)"
        x  -> TT.init x
    cur = maybe "-" (pack . show . (+1)) (st^.visibleEntries.L.listSelectedL)
    total = pack $ show $ Vec.length $ st^.visibleEntries.L.listElementsL

drawBorderColor :: State -> Widget Field -> Widget Field
drawBorderColor st = res
  where
    borderColor = case F.focusGetCurrent (st ^. focusRing) of
      Just BrowserField -> V.blue
      _                 -> V.black
    res = updateAttrMap (A.applyAttrMappings [(B.borderAttr, fg borderColor)])

listDrawElement :: (Show a) => Bool -> a -> Widget Field
listDrawElement _ a = str $ show a

drawBrowserListInner :: State -> Widget Field
drawBrowserListInner st =
  L.renderList listDrawElement True (st^.visibleEntries)
