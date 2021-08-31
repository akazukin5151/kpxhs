{-# LANGUAGE OverloadedStrings #-}

module Config.Config (parseConfig) where

import qualified Brick                         as B
import qualified Brick.Focus                   as F
import           Brick.Util                    (on)
import           Control.Exception             (IOException)
import           Control.Exception.Base        (catch)
import           Data.Bifunctor                (second)
import qualified Data.ByteString               as B
import           Data.Functor                  ((<&>))
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text, unpack)
import           Data.Text.Encoding            (decodeUtf8')
import           Data.Word                     (Word8)
import           Graphics.Vty                  (Color (ISOColor))
import           Graphics.Vty.Attributes       (withStyle)
import           Graphics.Vty.Attributes.Color (rgbColor)
import           Text.Read                     (readMaybe)

import Config.Defaults (defaultConfig, defaultTheme)
import Config.Types
    ( ActualAttr
    , ActualTheme
    , Attr (..)
    , ColorAux (..)
    , Config (dbPath, keyfilePath, timeout)
    , UserFacingAttr
    , StyleAux (..)
    , Timeout (DoNotClear, Seconds)
    )
import Types           (Field (KeyfileField, PasswordField, PathField))


fallback :: IOException -> IO B.ByteString
fallback _ = pure ""

parseConfig :: String -> IO (Maybe Int, Text, Text, F.FocusRing Field, ActualTheme)
parseConfig cfgdir = do
  file <- catch (B.readFile $ cfgdir <> "config.hs") fallback
  attrMap <- parseTheme $ cfgdir <> "theme.hs"
  let config = either
        (const defaultConfig)
        (fromMaybe defaultConfig . readMaybe . unpack)
        (decodeUtf8' file)
  let db_path  = fromMaybe "" (dbPath config)
  let kf_path  = fromMaybe "" (keyfilePath config)
  let ring     = if db_path == "" then pathfirst else passwordfirst
  let timeout' = timeoutToMaybe $ fromMaybe (Seconds 10) (timeout config)
  pure (timeout', db_path, kf_path, ring, attrMap)
  where
    pathfirst = F.focusRing [PathField, PasswordField, KeyfileField]
    passwordfirst = F.focusRing [PasswordField, KeyfileField, PathField]
    timeoutToMaybe (Seconds t) = Just t
    timeoutToMaybe DoNotClear  = Nothing

evalStyle :: StyleAux -> Word8
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
evalColor :: ColorAux -> Maybe Color
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

evalColorAttr :: Maybe Color -> Maybe Color -> ActualAttr
evalColorAttr (Just f) (Just b) = f `on` b
evalColorAttr (Just f) _        = B.fg f
evalColorAttr _        (Just b) = B.bg b
evalColorAttr _        _        = mempty

eval :: UserFacingAttr -> ActualAttr
eval r = res
  where
    mfg = evalColor (fg r)
    mbg = evalColor (bg r)
    colors = evalColorAttr mfg mbg
    g style acc = withStyle acc (evalStyle style)
    res = case styles r of
            [] -> colors
            xs -> foldr g colors xs

parseTheme :: FilePath -> IO ActualTheme
parseTheme theme_path = do
  file <- catch (B.readFile theme_path) fallback
  let theme_aux = either
        (const defaultTheme)
        (fromMaybe defaultTheme . readMaybe . unpack)
        (decodeUtf8' file)
  -- second eval === (\(a, b) -> (a, eval b))
  pure $ theme_aux <&> second eval
