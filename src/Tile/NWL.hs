{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Tile.NWL where

import Data.TileSet
import Tile
import Tile.Legend
import Tile.Neighborhood
import Tile.Wang

data NWLTileSets = NWLTileSets
  { neighborhoods :: TileSet Neighborhood
  , wangs         :: TileSet Wang
  , legends       :: TileSet Legend
  } deriving (Eq,Show)

instance HasTileSet Neighborhood NWLTileSets where
  getTileSet = neighborhoods

instance HasTileSet Wang NWLTileSets where
  getTileSet = wangs

instance HasTileSet Legend NWLTileSets where
  getTileSet = legends

type NWL = Wang :.: Neighborhood :.: Legend

lookupNW :: NWLTileSets -> Params NWL -> Maybe NWL
lookupNW = lookupTile

