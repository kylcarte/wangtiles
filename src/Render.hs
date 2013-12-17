{-# LANGUAGE TypeFamilies #-}

module Render
  ( module Render
  , module Graphics.Gloss.Data.Picture
  , fromImageRGBA8
  ) where

import Data.Points
import Data.TileMaps
import Data.TileSet
import Texture

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Juicy

class RenderTile p where
  type RenderData p
  buildRender   :: (Ord c) => TileMaps c -> Coord c -> Maybe p
  renderPicture :: RenderData p -> p -> Picture

type Textures = TileSet Texture

