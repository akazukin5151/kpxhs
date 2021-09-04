{-# LANGUAGE OverloadedStrings #-}

module UI.BrowserUI (drawBrowser) where

import           Brick                (Padding (Pad), padLeft)
import qualified Brick.AttrMap        as A
import qualified Brick.Focus          as F
import           Brick.Markup         (markup, (@?))
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
import           Data.Functor         ((<&>))
import           Data.Maybe           (fromMaybe)
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
    , visibleEntries, currentCmd
    )
import UI.Common (getEditor)


drawBrowser :: State -> [Widget Field]
drawBrowser st = [ui]
  where
    ui =
      C.vCenter $
        vBox
          [  C.hCenter $ drawHeader st,
             C.hCenter $ drawBrowserList st,
             C.hCenter $ st ^. footer
          ]

drawHeader :: State -> Widget Field
drawHeader st = drawSearchBox st <+> drawCmd st

drawCmd :: State -> Widget Field
drawCmd st = padLeft (Pad 2) $ str x
  where
    x = case st^.currentCmd of
          "" -> " "
          y  -> y

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

drawBrowserListInner :: State -> Widget Field
drawBrowserListInner st =
  L.renderListWithIndex (drawLine st) True (st^.visibleEntries)

drawLine :: State -> Int -> Bool -> TT.Text -> Widget n
drawLine st i isCurrent x = num <+> txt (" " <> x)
  where
    num = markup $ marker @? ("kpxhs" <> "line_number")
    marker = if isCurrent then "> " else diff
    diff =
      st^.visibleEntries . L.listSelectedL
      <&> abs . (i -)
      <&> (\d -> (if d >= 10 then "" else " ") <> show d)
      <&> TT.pack
      & fromMaybe " "

drawBrowserLabel :: State -> Widget Field -> Widget Field
drawBrowserLabel st = B.borderWithLabel label
  where
    label = str $ currentPath_ <> " (" <> cur <> "/" <> total <> ")"
    currentPath_ =
      case pathToStr $ st^.currentPath of
        "" -> "(Root)"
        x  -> TT.unpack $ TT.init x
    cur = maybe "-" (show . (+1)) (st^.visibleEntries.L.listSelectedL)
    total = show $ Vec.length $ st^.visibleEntries.L.listElementsL

drawBorderColor :: State -> Widget Field -> Widget Field
drawBorderColor st = res
  where
    borderColor = case F.focusGetCurrent (st ^. focusRing) of
      Just BrowserField -> V.blue
      _                 -> V.black
    res = updateAttrMap (A.applyAttrMappings [(B.borderAttr, fg borderColor)])
