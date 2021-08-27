{-# LANGUAGE OverloadedStrings #-}

module Config (parseConfig) where

import           Brick                         (AttrName, bg, fg)
import           Brick.AttrMap                 (attrName)
import qualified Brick.Focus                   as F
import           Brick.Util                    (on)
import           Control.Exception             (IOException)
import           Control.Exception.Base        (catch)
import           Data.Bifunctor                (second)
import qualified Data.ByteString               as B
import           Data.Functor                  ((<&>))
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text, unpack)
import qualified Data.Text                     as TT
import           Data.Text.Encoding            (decodeUtf8')
import           Graphics.Vty                  (Attr, Color (ISOColor))
import           Graphics.Vty.Attributes       (withStyle)
import           Graphics.Vty.Attributes.Color (rgbColor)
import           Text.Read                     (readMaybe)

import Types
    ( AttrAux (Bg, Fg, On, WithStyle)
    , ColorAux (ISO, RGB)
    , Field (KeyfileField, PasswordField, PathField)
    , Setting (Setting, dbPath, keyfilePath, timeout)
    , Theme
    , ThemeAux
    )


fallback :: IOException -> IO B.ByteString
fallback _ = pure ""

parseConfig :: String -> IO (Maybe Int, Text, Text, F.FocusRing Field, Theme)
parseConfig cfgdir = do
  file <- catch (B.readFile $ cfgdir <> "config") fallback
  attrMap <- parseTheme $ cfgdir <> "theme.hs"
  case decodeUtf8' file of
    Left _ -> pure (Just 10, "", "", pathfirst, attrMap)
    Right utf8_file -> do
      let parsed   = parseSettings (TT.lines utf8_file) initial
      let timeout' = fromMaybe (Just 10) (timeout parsed)
      let db_path  = fromMaybe "" (dbPath parsed)
      let kf_path  = fromMaybe "" (keyfilePath parsed)
      let ring     = if db_path == "" then pathfirst else passwordfirst
      pure (timeout', db_path, kf_path, ring, attrMap)
  where
    initial = Setting { timeout     = Nothing
                      , dbPath      = Nothing
                      , keyfilePath = Nothing
                      }
    pathfirst = F.focusRing [PathField, PasswordField, KeyfileField]
    passwordfirst = F.focusRing [PasswordField, KeyfileField, PathField]

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

defaultTheme :: ThemeAux
defaultTheme =
  [ (mkAttrName ["list","selected"],   Fg (ISO 1))
  , (mkAttrName ["edit"],              On (ISO 0) (ISO 7))
  , (mkAttrName ["edit","focused"],    On (ISO 7) (ISO 4))
  , (mkAttrName ["dialog"],            On (ISO 7) (ISO 4))
  , (mkAttrName ["button"],            On (ISO 0) (ISO 7))
  , (mkAttrName ["button","selected"], Bg (ISO 3))
  , (mkAttrName ["key"],               Bg (ISO 7))
  , (mkAttrName ["label"],             Fg (ISO 0))
  , (mkAttrName ["progressComplete"],  On (ISO 7) (ISO 4))
  ]
    where
      -- Use attrName to convert String -> AttrName then
      -- mappend that with the accumulator
      mkAttrName :: [String] -> AttrName
      mkAttrName xs = foldr ((<>) . attrName) mempty xs


parseSettings :: [Text] -> Setting -> Setting
parseSettings []       s = s
parseSettings (x : xs) s =
  if TT.null key' || TT.null value'
    then parseSettings xs s
    else parseSettings xs $
      case key of
        "timeout"      -> s { timeout     = handleNegative $ textToMaybeInt value }
        "db_path"      -> s { dbPath      = Just value }
        "keyfile_path" -> s { keyfilePath = Just value }
        _              -> s
  where
    (key', value') = TT.breakOn "=" x
    key = TT.strip key'
    value = TT.strip $ TT.drop 1 $ TT.strip value'

textToMaybeInt :: Text -> Maybe Int
textToMaybeInt = readMaybe . TT.unpack

handleNegative :: Maybe Int -> Maybe (Maybe Int)
handleNegative (Just y) | y > 0 = Just (Just y)
handleNegative (Just _)         = Just Nothing
handleNegative _                = Nothing
