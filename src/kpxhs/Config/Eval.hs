module Config.Eval (eval, evalName) where

import           Brick                   (AttrName)
import qualified Brick                   as B
import           Brick.AttrMap           (attrName)
import           Brick.Util              (on)
import           Graphics.Vty            (withStyle)
import           Graphics.Vty.Attributes (Color (ISOColor), rgbColor)

import Config.Types
    ( ActualAttrVal
    , ActualColor
    , ActualStyle
    , Color (..)
    , Name (Name)
    , Style (..)
    , UserFacingColor
    , UserFacingStyle
    , UserFacingVal
    , Val (bg, fg, styles)
    )

evalStyle :: UserFacingStyle -> ActualStyle
evalStyle Standout      = 0x01
evalStyle Underline     = 0x02
evalStyle ReverseVideo  = 0x04
evalStyle Blink         = 0x08
evalStyle Dim           = 0x10
evalStyle Bold          = 0x20
evalStyle Italic        = 0x40
evalStyle Strikethrough = 0x40

-- | Evaluates the colors, especially converting RGB into a Color240 code
-- Note that rgbColor might throw an error; this is intended
evalColor :: UserFacingColor -> Maybe ActualColor
evalColor Black         = Just $ ISOColor 0
evalColor Red           = Just $ ISOColor 1
evalColor Green         = Just $ ISOColor 2
evalColor Yellow        = Just $ ISOColor 3
evalColor Blue          = Just $ ISOColor 4
evalColor Magenta       = Just $ ISOColor 5
evalColor Cyan          = Just $ ISOColor 6
evalColor White         = Just $ ISOColor 7
evalColor BrightBlack   = Just $ ISOColor 8
evalColor BrightRed     = Just $ ISOColor 9
evalColor BrightGreen   = Just $ ISOColor 10
evalColor BrightYellow  = Just $ ISOColor 11
evalColor BrightBlue    = Just $ ISOColor 12
evalColor BrightMagenta = Just $ ISOColor 13
evalColor BrightCyan    = Just $ ISOColor 14
evalColor BrightWhite   = Just $ ISOColor 15
evalColor (RGB r g b)   = Just $ rgbColor r g b
evalColor Def           = Nothing

evalColorAttr :: Maybe ActualColor -> Maybe ActualColor -> ActualAttrVal
evalColorAttr (Just f) (Just b) = f `on` b
evalColorAttr (Just f) _        = B.fg f
evalColorAttr _        (Just b) = B.bg b
evalColorAttr _        _        = mempty

evalName :: Name -> AttrName
evalName (Name n) = foldr (\x acc -> attrName x <> acc) mempty n

eval :: UserFacingVal -> ActualAttrVal
eval r = res
  where
    mfg = evalColor (fg r)
    mbg = evalColor (bg r)
    colors = evalColorAttr mfg mbg
    g style acc = withStyle acc (evalStyle style)
    res = case styles r of
            [] -> colors
            xs -> foldr g colors xs
