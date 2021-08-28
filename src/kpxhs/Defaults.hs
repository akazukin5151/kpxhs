{-# LANGUAGE OverloadedStrings #-}

module Defaults where

import Brick         (AttrName)
import Brick.AttrMap (attrName)

import Types
    ( AttrAux (Bg, Fg, On)
    , ColorAux (ISO)
    , Config (Config, dbPath, keyfilePath, timeout)
    , ThemeAux
    , Timeout (Seconds)
    )
import Data.Text (Text)

defaultConfig :: Config
defaultConfig = Config { timeout = Just (Seconds 10)
                       , dbPath = Just ""
                       , keyfilePath = Just ""
                       }

defaultConfigText :: Text
defaultConfigText =
  "Config { timeout = Just (Seconds 10)\n\
  \       , dbPath = Just \"\"\n\
  \       , keyfilePath = Just \"\"\n\
  \       }"


defaultTheme :: ThemeAux
defaultTheme =
  [ (mkAttrName ["list","selected"],   Fg (ISO 1))
  , (mkAttrName ["edit"],              On (ISO 0) (ISO 7))
  , (mkAttrName ["edit","focused"],    On (ISO 7) (ISO 4))
  , (mkAttrName ["dialog"],            On (ISO 7) (ISO 4))
  , (mkAttrName ["button"],            On (ISO 0) (ISO 7))
  , (mkAttrName ["button","selected"], Bg (ISO 3))
  , (mkAttrName ["key"],               Bg (ISO 7))
  , (mkAttrName ["label"],             Fg (ISO 0))
  , (mkAttrName ["progressComplete"],  On (ISO 7) (ISO 4))
  ]
    where
      -- Use attrName to convert String -> AttrName then
      -- mappend that with the accumulator
      mkAttrName :: [String] -> AttrName
      mkAttrName xs = foldr ((<>) . attrName) mempty xs

-- Alignments look off here but is actually fine due to the quote escapes
defaultThemeText :: Text
defaultThemeText =
  "[ (AttrName [\"list\",\"selected\"],   Fg (ISO 1))\n\
  \, (AttrName [\"edit\"],              On (ISO 0) (ISO 7))\n\
  \, (AttrName [\"edit\",\"focused\"],    On (ISO 7) (ISO 4))\n\
  \, (AttrName [\"dialog\"],            On (ISO 7) (ISO 4))\n\
  \, (AttrName [\"button\"],            On (ISO 0) (ISO 7))\n\
  \, (AttrName [\"button\",\"selected\"], Bg (ISO 3))\n\
  \, (AttrName [\"kpxhs\", \"key\"],      Bg (ISO 7))\n\
  \, (AttrName [\"kpxhs\", \"label\"],    Fg (ISO 0))\n\
  \, (AttrName [\"progressComplete\"],  On (ISO 7) (ISO 4))\n\
  \]"


help :: String
help = "kpxhs - Interactive Keepass database TUI viewer\n\
        \  Usage\n\
        \    kpxhs                  Start the program\n\
        \    kpxhs [-h | --help]    Show this help\n\
        \    kpxhs --write-config   Write the default configs to ~/.config/kpxhs/\n\n\
        \  TUI keybindings (in general)\n\
        \    Esc                    Quit\n\
        \    Tab                    Cycle focus\n\
        \    Enter                  Show entry details\n\
        \    u                      Copy username\n\
        \    p                      Copy password\n\
        \  Navigation\n\
        \    j, s                   Move down\n\
        \    k, w                   Move up\n\
        \    g                      Move to top\n\
        \    G                      Move to bottom\n\
        \    q                      Page up\n\
        \    e                      Page down"
