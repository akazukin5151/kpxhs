module Config.Types where

import Brick        (AttrName)
import Data.Text    (Text)
import Data.Word    (Word8)
import Graphics.Vty (Attr)


-- | An external representation of the theme, replacing @fg@, @bg@, @on@ and @withStyle@
-- functions with constructors which can simply be @read@ in
type ThemeAux = [(AttrName, AttrAux)]

-- | Actual representation of the theme, using Brick types
type Theme = [(AttrName, Attr)]

-- | A 'dumb' representation of the @fg@, @bg@, @on@, and @withStyle@ functions
data AttrAux = Fg ColorAux
             | Bg ColorAux
             | On ColorAux ColorAux
             | WithStyle AttrAux StyleAux
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
