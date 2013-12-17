{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Wang where

import Data.TileMap
import Data.TileMaps
import Data.TileSet
import Render
import Util

import Control.Applicative

newtype RenderWang = RenderWang
  { wangIndex :: TileIndex
  } deriving (Eq,Show)

instance RenderTile RenderWang where
  type RenderData RenderWang = Textures
  buildRender tms c = do
    tm <- tmsLookup tms "wang"
    RenderWang <$> tmLookup tm c
  renderPicture ts p = fromImageRGBA8
    $ tsIndex ts $ wangIndex p

