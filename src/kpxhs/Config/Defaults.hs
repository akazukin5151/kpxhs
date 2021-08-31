{-# LANGUAGE OverloadedStrings #-}

module Config.Defaults where

import Brick         (AttrName)
import Brick.AttrMap (attrName)
import Data.Text     (Text)

import Config.Types
    ( Attr (Attr, styles)
    , ColorAux (Black, Blue, Def, Red, White, Yellow)
    , Config (..)
    , ThemeAux
    , Timeout (Seconds)
    , bg
    , fg
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


defaultTheme :: ThemeAux
defaultTheme =
  [ (mkAttrName ["list","selected"],   Attr { fg = Red,   bg = Def,    styles = [] } )
  , (mkAttrName ["edit"],              Attr { fg = Black, bg = White,  styles = [] } )
  , (mkAttrName ["edit","focused"],    Attr { fg = White, bg = Blue,   styles = [] } )
  , (mkAttrName ["dialog"],            Attr { fg = White, bg = Blue,   styles = [] } )
  , (mkAttrName ["button"],            Attr { fg = Black, bg = White,  styles = [] } )
  , (mkAttrName ["button","selected"], Attr { fg = Def,   bg = Yellow, styles = [] } )
  , (mkAttrName ["kpxhs","key"],       Attr { fg = Def,   bg = White,  styles = [] } )
  , (mkAttrName ["kpxhs","label"],     Attr { fg = Black, bg = Def,    styles = [] } )
  , (mkAttrName ["progressComplete"],  Attr { fg = White, bg = Blue,   styles = [] } )
  ]
    where
      -- Use attrName to convert String -> AttrName then
      -- mappend that with the accumulator
      mkAttrName :: [String] -> AttrName
      mkAttrName xs = foldr ((<>) . attrName) mempty xs

-- Alignments look off here but is actually fine due to the quote escapes
defaultThemeText :: Text
defaultThemeText =
  "[ (AttrName [\"list\",\"selected\"],   Attr { fg = Red,   bg = Def,    styles = [] } )\n\
  \, (AttrName [\"edit\"],              Attr { fg = Black, bg = White,  styles = [] } )\n\
  \, (AttrName [\"edit\",\"focused\"],    Attr { fg = White, bg = Blue,   styles = [] } )\n\
  \, (AttrName [\"dialog\"],            Attr { fg = White, bg = Blue,   styles = [] } )\n\
  \, (AttrName [\"button\"],            Attr { fg = Black, bg = White,  styles = [] } )\n\
  \, (AttrName [\"button\",\"selected\"], Attr { fg = Def,   bg = Yellow, styles = [] } )\n\
  \, (AttrName [\"kpxhs\",\"key\"],       Attr { fg = Def,   bg = White,  styles = [] } )\n\
  \, (AttrName [\"kpxhs\",\"label\"],     Attr { fg = Black, bg = Def,    styles = [] } )\n\
  \, (AttrName [\"progressComplete\"],  Attr { fg = White, bg = Blue,   styles = [] } )\n\
  \]"
