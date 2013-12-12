{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet where

import Util

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import Data.Text (Text, unpack)
import Data.Traversable (traverse)

-- TileSetConfig {{{

data TileSetConfig = TileSetConfig
  { textureFile    :: FilePath
  , tileSize       :: Size
  , ignoreTiles    :: [Coord]
  } deriving (Eq,Show)

instance FromJSON TileSetConfig where
  parseJSON (Object o) = 
        TileSetConfig
    <$> o .:  "texture-file"
    <*> o .:  "tile-size"
    <*> o .:? "ignore-tiles" .!= []
    where
  parseJSON _ = mzero

getSize :: Object -> Text -> Parser (Int,Int)
getSize o f = do
  sz <- o .: f
  (,) <$> sz .: "width"
      <*> sz .: "height"

getCoords :: Object -> Parser (Int,Int)
getCoords o = do
  (,) <$> o .: "col"
      <*> o .: "row"

getIgnoreTiles :: Object -> Text -> Parser [(Int,Int)]
getIgnoreTiles o f = do
  ig <- o .:? f
  case ig of
    Nothing -> return []
    Just l  -> mapM getCoords l

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

