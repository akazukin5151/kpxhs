{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Defaults where

import Data.FileEmbed     (embedFile)
import Data.Text          (Text)
import Data.Text.Encoding (decodeUtf8)

import Config.Types
    ( Color (Black, Blue, Def, Green, Red, White, Yellow)
    , Config (..)
    , Style (Bold, Italic)
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


-- This is the single source of truth for the default theme
-- The docs and defaultThemeText relies on this
-- EMBED START
defaultTheme :: UserFacingTheme
defaultTheme =
  [ (["edit"],                          Val { fg = Black,  bg = White,  styles = [] })
  , (["edit","focused"],                Val { fg = White,  bg = Blue,   styles = [] })
  , (["dialog"],                        Val { fg = White,  bg = Blue,   styles = [] })
  , (["button"],                        Val { fg = Black,  bg = White,  styles = [] })
  , (["button","selected"],             Val { fg = Def,    bg = Yellow, styles = [] })
  , (["progressComplete"],              Val { fg = White,  bg = Blue,   styles = [] })
  , (["kpxhs","key"],                   Val { fg = Def,    bg = White,  styles = [] })
  , (["kpxhs","label"],                 Val { fg = Black,  bg = Def,    styles = [] })
  , (["kpxhs","line_number"],           Val { fg = Yellow, bg = Def,    styles = [] })
  , (["kpxhs","line_number","focused"], Val { fg = Red,    bg = Def,    styles = [Bold]})
  , (["kpxhs","list_border"],           Val { fg = Black,  bg = Def,    styles = [] })
  , (["kpxhs","list_border","focused"], Val { fg = Blue,   bg = Def,    styles = [] })
  , (["kpxhs","directory"],             Val { fg = Black,  bg = Def,    styles = [Bold]})
  , (["kpxhs","directory","focused"],   Val { fg = Red,    bg = Def,    styles = [Bold]})
  , (["kpxhs","go_up"],                 Val { fg = Green
                                            , bg = Def
                                            , styles = [Bold, Italic]
                                            })
  , (["kpxhs","go_up","focused"],       Val { fg = Blue
                                            , bg = Def
                                            , styles = [Bold, Italic]
                                            })
  , (["kpxhs","entry"],                 Val { fg = Black,  bg = Def,    styles = [] })
  , (["kpxhs","entry","focused"],       Val { fg = Red,    bg = Def,    styles = [] })
  ]
-- EMBED END

defaultThemeText :: Text
defaultThemeText =
  -- the docs has to be built BEFORE the program can be compiled
  -- Must compile (`make install`) in repo's root dir
  -- Warning: if decodeUtf8 fails, it fails at runtime (but that's a big if)
  decodeUtf8 $(embedFile "docs/out/default_theme.hs")
