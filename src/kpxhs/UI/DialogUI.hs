module UI.DialogUI (drawDialog) where

import Brick.Types (Widget)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( str,
    vBox,
    (<+>),
    hLimitPercent,
  )

import Types
    ( dbPathField, keyfileField, passwordField, Field, State )
import Common ( footers )
import UI.Common ( getEditor )


hidePassword :: [String] -> String
hidePassword xs = replicate (length (unlines xs) - 1) '*'

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
            C.hCenter $ footers st
          ]
