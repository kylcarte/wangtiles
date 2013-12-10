{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet where

import Util hiding (Random(..))

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Traversable (traverse)

-- TileSetConfig {{{

data TileSetConfig = TileSetConfig
  { textureFile    :: FilePath
  , tileSize       :: (Int,Int)
  , ignoreTiles    :: [(Int,Int)]
  } deriving (Eq,Show)

instance FromJSON TileSetConfig where
  parseJSON (Object o) = 
        TileSetConfig
    <$> o .:               "texture-file"
    <*> o `getSize`        "tile-size"
    <*> o `getIgnoreTiles` "ignore-tiles"
    where
  parseJSON _ = mzero

getSize :: Object -> T.Text -> Parser (Int,Int)
getSize o f = do
  sz <- o .: f
  (,) <$> sz .: "width"
      <*> sz .: "height"

getCoords :: Object -> Parser (Int,Int)
getCoords o = do
  (,) <$> o .: "col"
      <*> o .: "row"

getIgnoreTiles :: Object -> T.Text -> Parser [(Int,Int)]
getIgnoreTiles o f = do
  ig <- o .:? f
  case ig of
    Nothing -> return []
    Just l  -> mapM getCoords l

loadTileSetConfig :: FilePath -> IO TileSetConfig
loadTileSetConfig = decodeFile

-- }}}

type TileSets = M.Map T.Text TileSetConfig

loadTileSets :: FilePath -> IO TileSets
loadTileSets f = decodeFile f >>= traverse loadTileSetConfig

lookupTileSetConfig :: T.Text -> TileSets -> IO TileSetConfig
lookupTileSetConfig = lookupIO

