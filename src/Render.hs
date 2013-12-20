{-# LANGUAGE OverloadedStrings #-}

module Render where

import Data.Points
import Data.TileSet
import Error
import Texture
import Tile.Load
import Util

import Data.Text (Text, unpack)

selectTileFor :: (CoordType c) => Text -> TileData c -> [(Text,Textures)]
  -> TileIndex -> Coord c -> Error Texture
selectTileFor setName td tex leg cd = wrapFail err $ do
  nm <- tdLegendName td leg
  ts <- errorLookup' tex nm
  wi <- tileIndexAt td cd setName
  tsLookup' ts wi
  where
  err = "selectTileFor" ++ unpack setName

selectWangTile, selectNeighborhoodTile :: (CoordType c)
  => TileData c -> [(Text,Textures)]
  -> TileIndex -> Coord c -> Error Texture
selectWangTile = selectTileFor "wang"
selectNeighborhoodTile = selectTileFor "neighborhood"

