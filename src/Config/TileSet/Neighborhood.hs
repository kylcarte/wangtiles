{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet.Neighborhood where

import Config.TileSet
import Tile
import Tile.Neighborhood
import Texture

loadNeighborhoodTileSet :: TileSetConfig
  -> TileSet Neighborhood -> IO NeighborhoodTileSet
loadNeighborhoodTileSet cfg ts = do
  texm <- loadTextureMap cfg
  maybe err return $ tsZipR texm ts
  where
  err = fail "TileSet mismatch"

