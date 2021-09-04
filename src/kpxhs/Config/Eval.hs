module Config.Eval (eval, evalName) where

import           Brick         (AttrName)
import qualified Brick         as B
import           Brick.AttrMap (attrName)
import           Brick.Util    (on)
import           Graphics.Vty
    ( black
    , blink
    , blue
    , bold
    , brightBlack
    , brightBlue
    , brightCyan
    , brightGreen
    , brightMagenta
    , brightRed
    , brightWhite
    , brightYellow
    , cyan
    , dim
    , green
    , italic
    , magenta
    , red
    , reverseVideo
    , rgbColor
    , standout
    , strikethrough
    , underline
    , white
    , withStyle
    , yellow
    )

import Config.Types
    ( ActualAttrVal
    , ActualColor
    , ActualStyle
    , Color (..)
    , Style (..)
    , UserFacingColor
    , UserFacingStyle
    , UserFacingVal
    , Val (bg, fg, styles)
    )

evalStyle :: UserFacingStyle -> ActualStyle
evalStyle Standout      = standout
evalStyle Underline     = underline
evalStyle ReverseVideo  = reverseVideo
evalStyle Blink         = blink
evalStyle Dim           = dim
evalStyle Bold          = bold
evalStyle Italic        = italic
evalStyle Strikethrough = strikethrough

-- | Evaluates the colors, especially converting RGB into a Color240 code
-- Note that rgbColor might throw an error; this is intended
evalColor :: UserFacingColor -> Maybe ActualColor
evalColor Black         = Just black
evalColor Red           = Just red
evalColor Green         = Just green
evalColor Yellow        = Just yellow
evalColor Blue          = Just blue
evalColor Magenta       = Just magenta
evalColor Cyan          = Just cyan
evalColor White         = Just white
evalColor BrightBlack   = Just brightBlack
evalColor BrightRed     = Just brightRed
evalColor BrightGreen   = Just brightGreen
evalColor BrightYellow  = Just brightYellow
evalColor BrightBlue    = Just brightBlue
evalColor BrightMagenta = Just brightMagenta
evalColor BrightCyan    = Just brightCyan
evalColor BrightWhite   = Just brightWhite
evalColor (RGB r g b)   = Just (rgbColor r g b)
evalColor Def           = Nothing

evalColorAttr :: Maybe ActualColor -> Maybe ActualColor -> ActualAttrVal
evalColorAttr (Just f) (Just b) = f `on` b
evalColorAttr (Just f) _        = B.fg f
evalColorAttr _        (Just b) = B.bg b
evalColorAttr _        _        = mempty

evalName :: [String] -> AttrName
evalName = foldr (\x acc -> attrName x <> acc) mempty

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
