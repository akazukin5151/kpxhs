{-# LANGUAGE OverloadedStrings #-}

module Config.Defaults where

import Brick         (AttrName)
import Brick.AttrMap (attrName)
import Data.Text     (Text)

import Config.Types
    ( AttrAux (Bg, Fg, On)
    , ColorAux (Black, Blue, Red, White, Yellow)
    , Config (..)
    , ThemeAux
    , Timeout (Seconds)
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
  [ (mkAttrName ["list","selected"],   Fg Red)
  , (mkAttrName ["edit"],              On Black White)
  , (mkAttrName ["edit","focused"],    On White Blue)
  , (mkAttrName ["dialog"],            On White Blue)
  , (mkAttrName ["button"],            On Black White)
  , (mkAttrName ["button","selected"], Bg Yellow)
  , (mkAttrName ["kpxhs","key"],       Bg White)
  , (mkAttrName ["kpxhs","label"],     Fg Black)
  , (mkAttrName ["progressComplete"],  On White Blue)
  ]
    where
      -- Use attrName to convert String -> AttrName then
      -- mappend that with the accumulator
      mkAttrName :: [String] -> AttrName
      mkAttrName xs = foldr ((<>) . attrName) mempty xs

-- Alignments look off here but is actually fine due to the quote escapes
defaultThemeText :: Text
defaultThemeText =
  "[ (AttrName [\"list\",\"selected\"],   Fg Red)\n\
  \, (AttrName [\"edit\"],              On Black White)\n\
  \, (AttrName [\"edit\",\"focused\"],    On White Blue)\n\
  \, (AttrName [\"dialog\"],            On White Blue)\n\
  \, (AttrName [\"button\"],            On Black White)\n\
  \, (AttrName [\"button\",\"selected\"], Bg Yellow)\n\
  \, (AttrName [\"kpxhs\", \"key\"],      Bg White)\n\
  \, (AttrName [\"kpxhs\", \"label\"],    Fg Black)\n\
  \, (AttrName [\"progressComplete\"],  On White Blue)\n\
  \]"
