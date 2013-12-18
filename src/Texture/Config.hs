{-# LANGUAGE OverloadedStrings #-}

module Texture.Config where

import Data.Points
import Util

import Control.Applicative
import Control.Monad
import Data.Aeson

class FromJSON c => TileSetConfig c where
  textureFile :: c -> FilePath
  tileSize    :: c -> Size Int
  ignoreTiles :: c -> [Coord Int]
  loadConfig  :: FilePath -> IO c
  loadConfig  = decodeFile

data StdConfig = StdConfig
  { stdTextureFile    :: FilePath
  , stdTileSize       :: Size Int
  , stdIgnoreTiles    :: [Coord Int]
  } deriving (Eq,Show)

instance TileSetConfig StdConfig where
  textureFile = stdTextureFile
  tileSize    = stdTileSize
  ignoreTiles = stdIgnoreTiles

instance FromJSON StdConfig where
  parseJSON (Object o) = 
        StdConfig
    <$> o .:  "texture-file"
    <*> o .:  "tile-size"
    <*> o .:? "ignore-tiles" .!= []
    where
  parseJSON _ = mzero

