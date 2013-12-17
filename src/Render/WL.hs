{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.WL where

import Data.TileMap
import Data.TileMaps
import Data.TileSet
import Render
import Util

import Control.Applicative

data RenderWL = RenderWL
  { wangIndex   :: TileIndex
  , legendIndex :: TileIndex
  } deriving (Eq,Show)

instance RenderTile RenderWL where
  type RenderData RenderWL = TileSet Textures
  buildRender tms c = do
    wtm <- tmsLookup tms "wang"
    ltm <- tmsLookup tms "legend"
    RenderWL <$> tmLookup wtm c
             <*> tmLookup ltm c
  renderPicture tts p = fromImageRGBA8
    $ tsIndex ts $ wangIndex p
    where
    ts = tsIndex tts $ legendIndex p

