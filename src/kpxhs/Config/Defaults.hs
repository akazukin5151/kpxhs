{-# LANGUAGE OverloadedStrings #-}

module Config.Defaults where

import Brick         (AttrName)
import Brick.AttrMap (attrName)
import Data.Text     (Text)

import Config.Common (Name, wrapName)
import Config.Types
    ( Color (Black, Blue, Def, Red, White, Yellow)
    , Config (..)
    , Timeout (Seconds)
    , UserFacingTheme
    , Val (Val, styles)
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
      mkAttrName' :: [String] -> AttrName
      mkAttrName' xs = foldr ((<>) . attrName) mempty xs
      mkAttrName :: [String] -> Name
      mkAttrName = wrapName . mkAttrName'

-- Alignments look off here but is actually fine due to the quote escapes
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
