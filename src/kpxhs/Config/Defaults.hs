{-# LANGUAGE OverloadedStrings #-}

module Config.Defaults where

import Data.Text (Text)

import Config.Types
    ( Color (Black, Blue, Def, Red, White, Yellow)
    , Config (..)
    , Style (Bold)
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
  [ (["edit"],                          Val { fg = Black,  bg = White,  styles = [] })
  , (["edit","focused"],                Val { fg = White,  bg = Blue,   styles = [] })
  , (["dialog"],                        Val { fg = White,  bg = Blue,   styles = [] })
  , (["button"],                        Val { fg = Black,  bg = White,  styles = [] })
  , (["button","selected"],             Val { fg = Def,    bg = Yellow, styles = [] })
  , (["progressComplete"],              Val { fg = White,  bg = Blue,   styles = [] })
  , (["kpxhs","key"],                   Val { fg = Def,    bg = White,  styles = [] })
  , (["kpxhs","label"],                 Val { fg = Black,  bg = Def,    styles = [] })
  , (["kpxhs","line_number"],           Val { fg = Yellow, bg = Def,    styles = [] })
  , (["kpxhs","list_border"],           Val { fg = Black,  bg = Def,    styles = [] })
  , (["kpxhs","list_border","focused"], Val { fg = Blue,   bg = Def,    styles = [] })
  , (["kpxhs","directory"],             Val { fg = Black,  bg = Def,    styles = [Bold]})
  , (["kpxhs","directory","focused"],   Val { fg = Red,    bg = Def,    styles = [Bold]})
  , (["kpxhs","entry"],                 Val { fg = Black,  bg = Def,    styles = [] } )
  , (["kpxhs","entry","focused"],       Val { fg = Red,    bg = Def,    styles = [] } )
  ]

-- Alignments look off here but is actually fine due to the quote escapes
-- Not using `show defaultTheme` because of manual formatting and newlines
defaultThemeText :: Text
defaultThemeText =
  "[ ([\"edit\"],                          Val { fg = Black,  bg = White,  styles = [] })\n\
  \, ([\"edit\",\"focused\"],                Val { fg = White,  bg = Blue,   styles = [] })\n\
  \, ([\"dialog\"],                        Val { fg = White,  bg = Blue,   styles = [] })\n\
  \, ([\"button\"],                        Val { fg = Black,  bg = White,  styles = [] })\n\
  \, ([\"button\",\"selected\"],             Val { fg = Def,    bg = Yellow, styles = [] })\n\
  \, ([\"progressComplete\"],              Val { fg = White,  bg = Blue,   styles = [] })\n\
  \, ([\"kpxhs\",\"key\"],                   Val { fg = Def,    bg = White,  styles = [] })\n\
  \, ([\"kpxhs\",\"label\"],                 Val { fg = Black,  bg = Def,    styles = [] })\n\
  \, ([\"kpxhs\",\"line_number\"],           Val { fg = Yellow, bg = Def,    styles = [] })\n\
  \, ([\"kpxhs\",\"list_border\"],           Val { fg = Black,  bg = Def,    styles = [] })\n\
  \, ([\"kpxhs\",\"list_border\",\"focused\"], Val { fg = Blue,   bg = Def,    styles = [] })\n\
  \, ([\"kpxhs\",\"directory\"],             Val { fg = Black,  bg = Def,    styles = [Bold]})\n\
  \, ([\"kpxhs\",\"directory\",\"focused\"],   Val { fg = Red,    bg = Def,    styles = [Bold]})\n\
  \, ([\"kpxhs\",\"entry\"],                 Val { fg = Black,  bg = Def,    styles = [] } )\n\
  \, ([\"kpxhs\",\"entry\",\"focused\"],       Val { fg = Red,    bg = Def,    styles = [] } )\n\
  \]"

