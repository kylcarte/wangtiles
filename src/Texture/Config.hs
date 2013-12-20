{-# LANGUAGE OverloadedStrings #-}

module Texture.Config where

import Data.Points
import Util.JSON

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import Data.Traversable (traverse)

class FromJSON c => TextureConfig c where
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

instance TextureConfig StdConfig where
  textureFile = stdTextureFile
  tileSize    = stdTileSize
  ignoreTiles = stdIgnoreTiles

instance FromJSON StdConfig where
  parseJSON (Object o) = StdConfig
    <$> o .:  "texture-file"
    <*> o .:  "tile-size"
    <*> o .:? "ignore-tiles" .!= []
  parseJSON _ = mzero

type TextureConfigs c = M.Map Text c

loadConfigs :: TextureConfig c => FilePath -> IO (TextureConfigs c)
loadConfigs = traverse loadConfig <=< decodeFile

loadStdConfigs :: FilePath -> IO (TextureConfigs StdConfig)
loadStdConfigs = loadConfigs

lookupConfig :: TextureConfig c => TextureConfigs c -> Text -> Maybe c
lookupConfig = flip M.lookup

