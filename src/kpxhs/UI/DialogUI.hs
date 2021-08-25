{-# LANGUAGE OverloadedStrings #-}

module UI.DialogUI (drawDialog) where

import           Brick.Types          (Widget)
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (hLimitPercent, str, vBox, (<+>))
import           Data.Text            (Text)
import qualified Data.Text            as TT
import           Lens.Micro           ((^.))

import Types
    ( Field
    , State
    , dbPathField
    , footer
    , keyfileField
    , passwordField
    )
import UI.Common (getEditor)


hidePassword :: [Text] -> Text
hidePassword xs = TT.replicate (TT.length (TT.unlines xs) - 1) "*"

drawDialog :: State -> [Widget Field]
drawDialog st = [ui]
  where
    e1 = getEditor st dbPathField TT.unlines
    e2 = getEditor st passwordField hidePassword
    e3 = getEditor st keyfileField TT.unlines
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
            C.hCenter $ st ^. footer
          ]
