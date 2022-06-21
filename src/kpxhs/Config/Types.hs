module Config.Types where

import           Brick.AttrMap (AttrName)
import           Data.Text     (Text)
import           Data.Word     (Word8)
import qualified Graphics.Vty  as V

-- | Some type aliases to better distinguish user facing types
-- and actual types (user-facing type names are easier to type)
type UserFacingVal = Val
type ActualAttrVal = V.Attr

type UserFacingColor = Color
type ActualColor = V.Color

type UserFacingStyle = Style
type ActualStyle = V.Style

-- | An external representation of the theme
-- (a mapping between attributes and styles)
type UserFacingTheme = [([String], Val)]

-- | Actual representation of the theme, using Brick types
type ActualTheme = [(AttrName, ActualAttrVal)]

-- An external representation of an attribute
data Val =
  Val { fg     :: Color
      , bg     :: Color
      , styles :: [Style]
      }
      deriving (Show, Read)

-- | An external representation of either an ISO color (code) or an RGB color
-- Needs to be converted into a Vty Color
-- This is because the Vty Color240 is extremely weird
data Color = Black
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
data Style = Standout
           | Underline
           | ReverseVideo
           | Blink
           | Dim
           | Bold
           | Italic
           | Strikethrough
           deriving (Show, Read)

-- Isomorphic to Maybe, used for users to make the nested maybe
-- less confusing
data Timeout = Seconds Int | DoNotClear
  deriving (Show, Read)

data Config = Config { timeout     :: Maybe Timeout
                     , dbPath      :: Maybe Text
                     , keyfilePath :: Maybe Text
                     , focusSearchOnStart :: Maybe Bool
                     } deriving (Show, Read)
