{-# LANGUAGE OverloadedStrings #-}

module Config.Defaults where

import Brick         (AttrName)
import Brick.AttrMap (attrName)
import Data.Text     (Text)

import Config.Types
    ( Color (Black, Blue, Def, Red, White, Yellow)
    , Config (..)
    , Timeout (Seconds)
    , UserFacingTheme
    , Val (Val, styles)
    , bg
    , fg
    , Name (Name)
    )

defaultConfig :: Config
defaultConfig = Config { timeout = Just (Seconds 10)
                       , dbPath = Nothing
                       , keyfilePath = Nothing
                       }

defaultConfigText :: Text
defaultConfigText =
  "Config { timeout = Just (Seconds 10)\n\
  \       , dbPath = Nothing\n\
  \       , keyfilePath = Nothing\n\
  \       }"


defaultTheme :: UserFacingTheme
defaultTheme =
  [ (Name ["list","selected"],   Val { fg = Red,   bg = Def,    styles = [] } )
  , (Name ["edit"],              Val { fg = Black, bg = White,  styles = [] } )
  , (Name ["edit","focused"],    Val { fg = White, bg = Blue,   styles = [] } )
  , (Name ["dialog"],            Val { fg = White, bg = Blue,   styles = [] } )
  , (Name ["button"],            Val { fg = Black, bg = White,  styles = [] } )
  , (Name ["button","selected"], Val { fg = Def,   bg = Yellow, styles = [] } )
  , (Name ["kpxhs","key"],       Val { fg = Def,   bg = White,  styles = [] } )
  , (Name ["kpxhs","label"],     Val { fg = Black, bg = Def,    styles = [] } )
  , (Name ["progressComplete"],  Val { fg = White, bg = Blue,   styles = [] } )
  ]

-- Alignments look off here but is actually fine due to the quote escapes
-- Not using `show defaultTheme` because of manual formatting and newlines
defaultThemeText :: Text
defaultThemeText =
  "[ (Name [\"list\",\"selected\"],   Val { fg = Red,   bg = Def,    styles = [] } )\n\
  \, (Name [\"edit\"],              Val { fg = Black, bg = White,  styles = [] } )\n\
  \, (Name [\"edit\",\"focused\"],    Val { fg = White, bg = Blue,   styles = [] } )\n\
  \, (Name [\"dialog\"],            Val { fg = White, bg = Blue,   styles = [] } )\n\
  \, (Name [\"button\"],            Val { fg = Black, bg = White,  styles = [] } )\n\
  \, (Name [\"button\",\"selected\"], Val { fg = Def,   bg = Yellow, styles = [] } )\n\
  \, (Name [\"kpxhs\",\"key\"],       Val { fg = Def,   bg = White,  styles = [] } )\n\
  \, (Name [\"kpxhs\",\"label\"],     Val { fg = Black, bg = Def,    styles = [] } )\n\
  \, (Name [\"progressComplete\"],  Val { fg = White, bg = Blue,   styles = [] } )\n\
  \]"
