{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Config.Render where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Char (isDigit,isHexDigit,toLower,digitToInt)
import Graphics.Gloss.Data.Color
import qualified Data.Text as T

data RenderConfig = RenderConfig
  { tileSpacing      :: Float
  , screenBorder     :: Float
  , screenSize       :: Int
  , windowBackground :: Color
  , renderTexture    :: Bool
  } deriving (Eq,Show)

instance FromJSON RenderConfig where
  parseJSON (Object o) =
        RenderConfig
    <$> o .:         "tile-spacing"
    <*> o .:         "screen-size"
    <*> o .:         "screen-border"
    <*> o `getColor` "window-background"
    <*> o .:         "render-texture"
  parseJSON _ = mzero

defaultRenderConfig :: RenderConfig
defaultRenderConfig = RenderConfig
  { tileSpacing      = 0
  , screenBorder     = 20
  , screenSize       = 600
  , windowBackground = white
  , renderTexture    = True
  }

hexDigitToInt :: Char -> Int
hexDigitToInt c =
  case toLower c of
    'a' -> 10
    'b' -> 11
    'c' -> 12
    'd' -> 13
    'e' -> 14
    'f' -> 15
    _ | isDigit c -> digitToInt c
    _ -> error $ "Not a hex digit: " ++ [c]

addHexDigits :: (Char,Char) -> Int
addHexDigits (a,b) = hexDigitToInt a * 16 + hexDigitToInt b

getColor :: Object -> T.Text -> Parser Color
getColor o f = do
  ct <- o .: f
  case ct of
    "white"      -> return white
    "black"      -> return black
    "red"        -> return red
    "green"      -> return green
    "blue"       -> return blue
    "yellow"     -> return yellow
    "cyan"       -> return cyan
    "magenta"    -> return magenta
    "rose"       -> return rose
    "violet"     -> return violet
    "azure"      -> return azure
    "aquamarine" -> return aquamarine
    "chartreuse" -> return chartreuse
    "orange"     -> return orange
    '#' : cs@[r1,r2,g1,g2,b1,b2,a1,a2]
      | all isHexDigit cs
      -> return $ makeColor r g b a
         where
         r = toEnum $ addHexDigits (r1,r2)
         g = toEnum $ addHexDigits (g1,g2)
         b = toEnum $ addHexDigits (b1,b2)
         a = toEnum $ addHexDigits (a1,a2)
    _ -> mzero

