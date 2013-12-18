{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Tile where

import Data.Points
import Data.TileMap
import Data.TileSet

class (Monad m) => TileLogic m t | t -> m where
  fillTileMap :: (Integral c) => TileSet t -> TileMap c -> m (TileMap c)
  ppTile :: t -> String

-- Examples {{{

neigh8TM :: TileMap Int
neigh8TM = mkIotaTileMapExcept (mkSize 10 5) [(0,3),(0,4),(1,4)]

-- }}}

