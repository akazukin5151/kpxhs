module Config.Types where

import Brick        (AttrName)
import Data.Text    (Text)
import Data.Word    (Word8)
import qualified Graphics.Vty as V


-- | An external representation of the theme
-- (a mapping between attributes and styles)
type ThemeAux = [(AttrName, Attr)]

-- | Actual representation of the theme, using Brick types
type ActualTheme = [(AttrName, ActualAttr)]

-- | Some type aliases to better distinguish the two
-- (Attr is used as the user-facing name because it's easier to type)
type ExternalAttr = Attr
type ActualAttr = V.Attr

-- An external representation of an attribute
data Attr =
  Attr { fg     :: ColorAux
       , bg     :: ColorAux
       , styles :: [StyleAux]
       }
       deriving (Show, Read)

-- | An external representation of either an ISO color (code) or an RGB color
-- Needs to be converted into a Vty Color
-- This is because the Vty Color240 is extremely weird
data ColorAux = Black
              | Red
              | Green
              | Yellow
              | Blue
              | Magenta
              | Cyan
              | White
              | BrightBlack
              | BrightRed
              | BrightGreen
              | BrightYellow
              | BrightBlue
              | BrightMagenta
              | BrightCyan
              | BrightWhite
              | RGB Word8 Word8 Word8
              | Def
              deriving (Show, Read)

-- | An external representation of the text styles available
data StyleAux = Standout
              | Underline
              | ReverseVideo
              | Blink
              | Dim
              | Bold
              | Italic
              | Strikethrough
              deriving (Show, Read)

data Timeout = Seconds Int | DoNotClear
  deriving (Show, Read)

data Config = Config { timeout     :: Maybe Timeout
                     , dbPath      :: Maybe Text
                     , keyfilePath :: Maybe Text
                     } deriving (Show, Read)
