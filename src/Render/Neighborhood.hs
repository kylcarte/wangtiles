{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Neighborhood where

import Data.TileMap
import Data.TileMaps
import Data.TileSet
import Render
import Util

import Control.Applicative

newtype RenderNeighborhood = RenderNeighborhood
  { neighborhoodIndex :: TileIndex
  } deriving (Eq,Show)

instance RenderTile RenderNeighborhood where
  type RenderData RenderNeighborhood = Textures
  buildRender tms c = do
    tm <- tmsLookup tms "neighborhood"
    RenderNeighborhood <$> tmLookup tm c
  renderPicture ts p = fromImageRGBA8
    $ tsIndex ts $ neighborhoodIndex p

