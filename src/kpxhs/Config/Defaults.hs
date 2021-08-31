{-# LANGUAGE OverloadedStrings #-}

module Config.Defaults where

import Brick         (AttrName)
import Brick.AttrMap (attrName)
import Data.Text     (Text)

import Config.Types
    ( Val (Val, styles)
    , Color (Black, Blue, Def, Red, White, Yellow)
    , Config (..)
    , Timeout (Seconds)
    , UserFacingTheme
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


defaultTheme :: UserFacingTheme
defaultTheme =
  [ (mkAttrName ["list","selected"],   Val { fg = Red,   bg = Def,    styles = [] } )
  , (mkAttrName ["edit"],              Val { fg = Black, bg = White,  styles = [] } )
  , (mkAttrName ["edit","focused"],    Val { fg = White, bg = Blue,   styles = [] } )
  , (mkAttrName ["dialog"],            Val { fg = White, bg = Blue,   styles = [] } )
  , (mkAttrName ["button"],            Val { fg = Black, bg = White,  styles = [] } )
  , (mkAttrName ["button","selected"], Val { fg = Def,   bg = Yellow, styles = [] } )
  , (mkAttrName ["kpxhs","key"],       Val { fg = Def,   bg = White,  styles = [] } )
  , (mkAttrName ["kpxhs","label"],     Val { fg = Black, bg = Def,    styles = [] } )
  , (mkAttrName ["progressComplete"],  Val { fg = White, bg = Blue,   styles = [] } )
  ]
    where
      -- Use attrName to convert String -> AttrName then
      -- mappend that with the accumulator
      mkAttrName :: [String] -> AttrName
      mkAttrName xs = foldr ((<>) . attrName) mempty xs

-- Alignments look off here but is actually fine due to the quote escapes
defaultThemeText :: Text
defaultThemeText =
  "[ (AttrName [\"list\",\"selected\"],   Val { fg = Red,   bg = Def,    styles = [] } )\n\
  \, (AttrName [\"edit\"],              Val { fg = Black, bg = White,  styles = [] } )\n\
  \, (AttrName [\"edit\",\"focused\"],    Val { fg = White, bg = Blue,   styles = [] } )\n\
  \, (AttrName [\"dialog\"],            Val { fg = White, bg = Blue,   styles = [] } )\n\
  \, (AttrName [\"button\"],            Val { fg = Black, bg = White,  styles = [] } )\n\
  \, (AttrName [\"button\",\"selected\"], Val { fg = Def,   bg = Yellow, styles = [] } )\n\
  \, (AttrName [\"kpxhs\",\"key\"],       Val { fg = Def,   bg = White,  styles = [] } )\n\
  \, (AttrName [\"kpxhs\",\"label\"],     Val { fg = Black, bg = Def,    styles = [] } )\n\
  \, (AttrName [\"progressComplete\"],  Val { fg = White, bg = Blue,   styles = [] } )\n\
  \]"
