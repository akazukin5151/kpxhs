{-# LANGUAGE OverloadedStrings #-}

module Config (parseConfig) where

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
import           Graphics.Vty                  (Attr, Color (ISOColor))
import           Graphics.Vty.Attributes       (withStyle)
import           Graphics.Vty.Attributes.Color (rgbColor)
import           Text.Read                     (readMaybe)

import Defaults (defaultTheme)
import Types
    ( AttrAux (Bg, Fg, On, WithStyle)
    , ColorAux (ISO, RGB)
    , Config (Config, dbPath, keyfilePath, timeout)
    , Field (KeyfileField, PasswordField, PathField)
    , Theme
    , Timeout (DoNotClear, Seconds)
    )


fallback :: IOException -> IO B.ByteString
fallback _ = pure ""

defaultConfig :: Config
defaultConfig = Config { timeout = Just (Seconds 10)
                       , dbPath = Just ""
                       , keyfilePath = Just ""
                       }

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

-- | Evaluates the colors, especially converting RGB into a Color240 code
-- Note that rgbColor might throw an error; this is intended
evalColor :: ColorAux -> Color
evalColor (ISO i)     = ISOColor i
evalColor (RGB r g b) = rgbColor r g b

-- | Evaluates the dumb representation using their respective functions
eval :: AttrAux -> Attr
eval (Fg c)          = fg (evalColor c)
eval (Bg c)          = bg (evalColor c)
eval (On f b)        = evalColor f `on` evalColor b
eval (WithStyle a s) = withStyle (eval a) s

parseTheme :: FilePath -> IO [(AttrName, Attr)]
parseTheme theme_path = do
  file <- catch (B.readFile theme_path) fallback
  let theme_aux = either
        (const defaultTheme)
        (fromMaybe defaultTheme . readMaybe . unpack)
        (decodeUtf8' file)
  -- second eval === (\(a, b) -> (a, eval b))
  pure $ theme_aux <&> second eval
