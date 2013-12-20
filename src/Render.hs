{-# LANGUAGE OverloadedStrings #-}

module Render where

import Data.Points
import Data.TileSet
import Error
import Texture
import Tile.Load
import Util

import Data.Text (Text, unpack)

selectTileFor :: (CoordType c) => Text -> [(Text,Textures)]
  -> TileData c -> TileIndex -> Coord c -> Error (Maybe Texture)
selectTileFor setName tex td leg cd = wrapFail err $ do
  nm <- tdLegendName td leg
  case dropError $ errorLookup' tex nm of
    Nothing -> return Nothing
    Just ts -> do
      wi <- tileIndexAt td cd setName
      return $ dropError $ tsLookup' ts wi
  where
  err = "selectTileFor '" ++ unpack setName ++ "'"

selectWangTile, selectNeighborhoodTile :: (CoordType c)
  => [(Text,Textures)] -> TileData c
  -> TileIndex -> Coord c -> Error (Maybe Texture)
selectWangTile = selectTileFor "wang"
selectNeighborhoodTile = selectTileFor "neighborhood"

