{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet.Neighborhood where

import Config.TileSet
import Data.TileSet
import Display.Neighborhood
import Tile.Neighborhood

import Data.Text (Text)

loadNeighborhoodTextureSet :: TileSets -> Text
  -> TileSet Neighborhood -> IO NeighborhoodTextureSet
loadNeighborhoodTextureSet tss n ts = do
  cfg <- lookupTileSetConfig tss n
  -- txm <- loadTextureMap cfg
  -- nts <- io errMsg $ tsZipR txm ts
  return $ mkNeighborhoodTextureSet cfg ts
  -- where
  -- errMsg = "Texture provided by TileSetConfig does not \
  --            \contain sufficient textures for Neighborhood TileSet"

