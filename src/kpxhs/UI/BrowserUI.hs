module UI.BrowserUI (drawBrowser) where

import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Types (Widget)
import Brick.Util (fg)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Brick.Widgets.Core
  ( str,
    updateAttrMap,
    vBox,
    (<+>),
    hLimitPercent,
    vLimitPercent,
  )

import Common
import Types
import UI.Common


drawBrowser :: State -> [Widget Field]
drawBrowser st = [ui]
  where
    ui =
      C.vCenter $
        vBox
          [  C.hCenter $ drawSearchBox st,
             C.hCenter $ drawBrowserList st,
             C.hCenter $ footers st
          ]

drawSearchBox :: State -> Widget Field
drawSearchBox st = str "Search: " <+> hLimitPercent 75 ed
  where
    ed = getEditor st searchField unlines

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
    label = foldr1 (<+>) $ str <$> [currentDir_, "(", cur, "/", total, ")"]
    currentDir_ = case dirsToStr $ st^.currentDir of
      "" -> "(Root) "
      x -> init x ++ " "
    cur = maybe "-" (\x -> show (x+1)) (st^.visibleEntries.L.listSelectedL)
    total = show $ Vec.length $ st^.visibleEntries.L.listElementsL

drawBorderColor :: State -> Widget Field -> Widget Field
drawBorderColor st = res
  where
    borderColor = case F.focusGetCurrent (st ^. focusRing) of
      Just BrowserField -> V.blue
      _ -> V.black
    res = updateAttrMap (A.applyAttrMappings [(B.borderAttr, fg borderColor)])

listDrawElement :: (Show a) => Bool -> a -> Widget Field
listDrawElement _ a = str $ show a

drawBrowserListInner :: State -> Widget Field
drawBrowserListInner st =
  L.renderList listDrawElement True (st^.visibleEntries)

