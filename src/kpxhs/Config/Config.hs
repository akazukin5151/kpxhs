{-# LANGUAGE OverloadedStrings #-}

module Config.Config (parseConfig) where

import           Brick                         (AttrName, bg, fg)
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
import           Graphics.Vty                  (Attr, Color (ISOColor))
import           Graphics.Vty.Attributes       (withStyle)
import           Graphics.Vty.Attributes.Color (rgbColor)
import           Text.Read                     (readMaybe)

import Config.Defaults (defaultConfig, defaultTheme)
import Config.Types
    ( AttrAux (..)
    , ColorAux (..)
    , Config (dbPath, keyfilePath, timeout)
    , StyleAux (..)
    , Theme
    , Timeout (DoNotClear, Seconds)
    )
import Types (Field (PathField, PasswordField, KeyfileField))


fallback :: IOException -> IO B.ByteString
fallback _ = pure ""

parseConfig :: String -> IO (Maybe Int, Text, Text, F.FocusRing Field, Theme)
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
evalColor :: ColorAux -> Color
evalColor Black         = ISOColor 0
evalColor Red           = ISOColor 1
evalColor Green         = ISOColor 2
evalColor Yellow        = ISOColor 3
evalColor Blue          = ISOColor 4
evalColor Magenta       = ISOColor 5
evalColor Cyan          = ISOColor 6
evalColor White         = ISOColor 7
evalColor BrightBlack   = ISOColor 8
evalColor BrightRed     = ISOColor 9
evalColor BrightGreen   = ISOColor 10
evalColor BrightYellow  = ISOColor 11
evalColor BrightBlue    = ISOColor 12
evalColor BrightMagenta = ISOColor 13
evalColor BrightCyan    = ISOColor 14
evalColor BrightWhite   = ISOColor 15
evalColor (RGB r g b)   = rgbColor r g b

-- | Evaluates the dumb representation using their respective functions
eval :: AttrAux -> Attr
eval (Fg c)          = fg (evalColor c)
eval (Bg c)          = bg (evalColor c)
eval (On f b)        = evalColor f `on` evalColor b
eval (WithStyle a s) = withStyle (eval a) (evalStyle s)
eval Empty           = mempty

parseTheme :: FilePath -> IO [(AttrName, Attr)]
parseTheme theme_path = do
  file <- catch (B.readFile theme_path) fallback
  let theme_aux = either
        (const defaultTheme)
        (fromMaybe defaultTheme . readMaybe . unpack)
        (decodeUtf8' file)
  -- second eval === (\(a, b) -> (a, eval b))
  pure $ theme_aux <&> second eval
