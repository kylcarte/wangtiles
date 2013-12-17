{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Legend where

import Data.TileMap
import Data.TileMaps
import Data.TileSet
import Render
import Util

import Control.Applicative

newtype RenderLegend = RenderLegend
  { legendIndex :: TileIndex
  } deriving (Eq,Show)

instance RenderTile RenderLegend where
  type RenderData RenderLegend = Textures
  buildRender tms c = do
    tm <- tmsLookup tms "legend"
    RenderLegend <$> tmLookup tm c
  renderPicture ts p = fromImageRGBA8
    $ tsIndex ts $ legendIndex p

