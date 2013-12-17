{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Render
  ( module Render
  , module Graphics.Gloss.Data.Picture
  ) where

import Data.TileSet
import Tile

import Graphics.Gloss.Data.Picture

data Proxy a = Proxy

class (TileLogic t, Monad m) => RenderTile m t where
  mkPictureProxy :: Proxy t -> Params t -> m Picture

type Textures = TileSet Picture

