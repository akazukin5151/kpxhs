{-# LANGUAGE OverloadedStrings #-}

module Config (parseConfig) where

import qualified Brick.Focus            as F
import           Control.Exception      (IOException)
import           Control.Exception.Base (catch)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as TT
import qualified Data.Text.IO           as TTI
import           Text.Read              (readMaybe)

import Types
    ( Field (KeyfileField, PasswordField, PathField)
    , Setting (Setting, dbPath, keyfilePath, timeout)
    )


parseConfig :: String -> IO (Int, Text, Text, F.FocusRing Field)
parseConfig cfgdir = do
  file <- catch (TTI.readFile cfgdir) fallback
  let parsed   = parse initial $ TT.lines file
  let timeout' = fromMaybe 10 (timeout parsed)
  let db_path  = fromMaybe "" (dbPath parsed)
  let kf_path  = fromMaybe "" (keyfilePath parsed)
  let ring     = if db_path == "" then pathfirst else passwordfirst
  pure (timeout', db_path, kf_path, ring)
  where
    fallback :: IOException -> IO Text
    fallback _ = pure ""
    initial = Setting { timeout = Nothing
                      , dbPath = Nothing
                      , keyfilePath = Nothing
                      }
    pathfirst = F.focusRing [PathField, PasswordField, KeyfileField]
    passwordfirst = F.focusRing [PasswordField, KeyfileField, PathField]

parse :: Setting -> [Text] -> Setting
parse s []       = s
parse s (x : xs) =
  if TT.null key' || TT.null value'
    then parse s xs
    else case key of
      "timeout"      -> parse s { timeout     = Just =<< textToMaybeInt value } xs
      "db_path"      -> parse s { dbPath      = Just value } xs
      "keyfile_path" -> parse s { keyfilePath = Just value } xs
      _              -> parse s xs
  where
    (key', value') = TT.breakOn "=" x
    key = TT.strip key'
    value = TT.strip $ TT.drop 1 $ TT.strip value'
    textToMaybeInt = readMaybe . TT.unpack
