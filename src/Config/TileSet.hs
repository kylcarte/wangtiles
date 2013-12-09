{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet where

import Tile
import Util hiding (Random(..))

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number (Number(..))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Traversable (traverse)

loadConfig :: FilePath -> IO TileSetConfig
loadConfig = decodeFile

data TileSetConfig = TileSetConfig
  { textureFile    :: FilePath
  , tileSize       :: (Int,Int)
  , neighborhood   :: Neighborhood
  , tileIndices :: TileSet (Int,Int)
  } deriving (Eq,Show)

instance FromJSON TileSetConfig where
  parseJSON (Object o) = 
        TileSetConfig
    <$> o .:               "texture-file"
    <*> o `getSize`        "tile-size"
    <*> o .:               "neighborhood"
    <*> o `getTileIndices` "tile-indices"
    where
  parseJSON _ = mzero

getSize :: Object -> T.Text -> Parser (Int,Int)
getSize o f = do
  sz <- o .: f
  (,) <$> sz .: "width"
      <*> sz .: "height"

getTileIndices :: Object -> T.Text -> Parser (TileSet (Int,Int))
getTileIndices o f = mkIndexMap <$> o .: f

data Neighborhood
  = N4
  | N8
  deriving (Eq,Show)

instance FromJSON Neighborhood where
  parseJSON (Number (I i)) = case i of
    4 -> return N4
    8 -> return N8
    _ -> fail $ "Unknown Neighborhood type: " ++ show i
  parseJSON _ = mzero

type TileSetMap = M.Map T.Text TileSetConfig

lookupSet :: T.Text -> TileSetMap -> IO TileSetConfig
lookupSet n tsm = case M.lookup n tsm of
  Just cfg  -> return cfg
  Nothing   -> fail $ "unknown tile set '" ++ T.unpack n ++ "'"

loadTileSetMap :: FilePath -> IO TileSetMap
loadTileSetMap f = decodeFile f >>= traverse decodeFile

