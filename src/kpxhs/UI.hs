{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module UI (drawUI) where

import Data.Maybe
import qualified Data.Text as TT
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Types (Widget)
import Brick.Util (fg)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Brick.Widgets.Table
import Brick.Widgets.Core
  ( str,
    txt,
    updateAttrMap,
    vBox,
    (<+>),
    hLimitPercent,
    vLimitPercent,
  )

import qualified Brick.Widgets.Dialog as D
import Brick.Widgets.Core (padAll)

import Common
import Types


drawUI :: State -> [Widget Field]
drawUI st = case st ^. activeView of
  PasswordView -> drawDialog st
  EntryView -> drawEntryDetails st
  ExitView -> drawExitView st
  _ -> drawBrowser st

drawDialog :: State -> [Widget Field]
drawDialog st = [ui]
  where
    e1 = getEditor st dbPathField unlines
    e2 = getEditor st passwordField hidePassword
    e3 = getEditor st keyfileField unlines
    ui =
      C.vCenter $
        vBox
          [
            C.hCenter $ str "kpxhs (GPLv3)",
            C.hCenter $ str " ",
            C.hCenter $ str "File:     " <+> hLimitPercent 75 e1,
            C.hCenter $ str " ",
            C.hCenter $ str "Password: " <+> hLimitPercent 75 e2,
            C.hCenter $ str " ",
            C.hCenter $ str "Keyfile:  " <+> hLimitPercent 75 e3,
            C.hCenter $ str " ",
            C.hCenter $ str (footers st)
          ]

getEditor :: State
          -> Getting (E.Editor String Field) State (E.Editor String Field)
          -> ([String] -> String)
          -> Widget Field
getEditor st f g =
  F.withFocusRing (st ^. focusRing) (E.renderEditor (str . g)) (st ^. f)


drawBrowser :: State -> [Widget Field]
drawBrowser st = [ui]
  where
    ui =
      C.vCenter $
        vBox
          [  C.hCenter $ drawSearchBox st,
             C.hCenter $ drawBrowserList st,
             C.hCenter $ str (footers st)
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


drawEntryDetails :: State -> [Widget Field]
drawEntryDetails st =
  [C.center $ vBox $ fromMaybe def (drawEntryDetailsInner st)]
  where
    def = [C.hCenter $ str "Failed to get entry!"]

drawEntryDetailsInner :: State -> Maybe [Widget Field]
drawEntryDetailsInner st = do
  entryData <- maybeGetEntryData st
  let tableWidget = drawTable $ TT.pack entryData
  pure [ C.hCenter $ renderTable $ table tableWidget,
         C.hCenter $ str $ st ^. footer ]

drawTable :: TT.Text -> [[Widget Field]]
drawTable text_ = restRows ++ notesRow
  where
    -- Do not split by ": " on notes
    [rest, notes] = TT.splitOn "Notes: " text_
    xs = TT.splitOn ": " <$> TT.lines rest
    restRows = (txt . replaceEmpty <$>) <$> xs
    notesRow = [[txt "Notes", txt $ replaceEmpty notes]]

replaceEmpty :: TT.Text -> TT.Text
replaceEmpty s =
  case s of
    "" -> "-"
    x -> x

drawExitView :: State -> [Widget Field]
drawExitView st = [ui]
  where
    ui = D.renderDialog (st^.exitDialog)
         $ C.hCenter
         $ padAll 1
         $ str "Do you want to clear the clipboard before exiting?"
