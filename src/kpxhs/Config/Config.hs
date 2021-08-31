{-# LANGUAGE OverloadedStrings #-}

module Config.Config (parseConfig) where

import qualified Brick.Focus            as F
import           Control.Exception      (IOException)
import           Control.Exception.Base (catch)
import           Data.Bifunctor         (Bifunctor (bimap))
import qualified Data.ByteString        as B
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text, unpack)
import           Data.Text.Encoding     (decodeUtf8')
import           Text.Read              (readMaybe)

import Config.Defaults (defaultConfig, defaultTheme)
import Config.Eval     (eval, evalName)
import Config.Types
    ( ActualTheme
    , Config (dbPath, keyfilePath, timeout)
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

-- type ActualTheme = [(AttrName, ActualAttrVal)]
parseTheme :: FilePath -> IO ActualTheme
parseTheme theme_path = do
  file <- catch (B.readFile theme_path) fallback
  let theme_aux = either
        (const defaultTheme)
        (fromMaybe defaultTheme . readMaybe . unpack)
        (decodeUtf8' file)
  -- bimap f g === (\(a, b) -> (f a, g b))
  pure $ bimap evalName eval <$> theme_aux
