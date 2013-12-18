{-# LANGUAGE TypeFamilies #-}

module Render where

import Data.Points
import Data.TileMaps
import Data.TileSet
import Texture

class RenderTile p where
  type RenderTable p
  buildRender   :: (Ord c) => TileMaps c -> Coord c -> Maybe p
  chooseTexture :: RenderTable p -> p -> Maybe Texture

type Textures = TileSet Texture

