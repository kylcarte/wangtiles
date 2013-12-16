{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet where

import Data.Points
import Util

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text, unpack)
import Data.Traversable (traverse)

-- TileSetConfig {{{

data TileSetConfig = TileSetConfig
  { textureFile    :: FilePath
  , tileSize       :: Size Int
  , ignoreTiles    :: [Coord Int]
  } deriving (Eq,Show)

instance FromJSON TileSetConfig where
  parseJSON (Object o) = 
        TileSetConfig
    <$> o .:  "texture-file"
    <*> o .:  "tile-size"
    <*> o .:? "ignore-tiles" .!= []
    where
  parseJSON _ = mzero

loadTileSetConfig :: FilePath -> IO TileSetConfig
loadTileSetConfig = decodeFile

-- }}}

type TileSets = M.Map Text TileSetConfig

loadTileSets :: FilePath -> IO TileSets
loadTileSets f = decodeFile f >>= traverse loadTileSetConfig

lookupTileSetConfig :: TileSets -> Text -> IO TileSetConfig
lookupTileSetConfig tss n = io errMsg $ M.lookup n tss
  where
  errMsg = "TileSet '" ++ unpack n ++ "' not configured"

loadTileSet :: FilePath -> Text -> IO TileSetConfig
loadTileSet f n = do
  tss <- loadTileSets f
  lookupTileSetConfig tss n

