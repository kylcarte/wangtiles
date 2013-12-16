{-# LANGUAGE MultiParamTypeClasses #-}

module Tile.NW where

import Data.TileSet
import Tile
import Tile.Neighborhood
import Tile.Wang

data NWTileSets = NWTileSets
  { neighborhoods :: TileSet Neighborhood
  , wangs         :: TileSet Wang
  } deriving (Eq,Show)

instance HasTileSet Neighborhood NWTileSets where
  getTileSet = neighborhoods

instance HasTileSet Wang NWTileSets where
  getTileSet = wangs

type NW = Wang :.: Neighborhood

lookupNW :: Params NW -> NWTileSets -> Maybe NW
lookupNW = lookupTile

