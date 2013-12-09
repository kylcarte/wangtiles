{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet.Wang where

import Config.TileSet
import Config.Render.Wang
import Tile.Wang
import Texture
import Util

import Data.Text (Text,unpack)
import qualified Data.Map as M

loadWangTileSet :: [Tile] -> TileSetConfig -> IO (WangTileSet,WangRenderConfig)
loadWangTileSet ts cfg = do
  gm <- loadTextureMap (tileSize cfg) (textureFile cfg)
  let mtex = mkTextureMap gm $ tileIndices cfg
  texm <- maybe (fail "Unreferenced textures") return mtex
  let (x,y) = tileSize cfg
      wts = tzip texm ts
      wrc = defaultWangRenderConfig
              { wTileSize   = (toEnum x,toEnum y)
              , wEdgeRadius = (/ 2)
              }
  return (wts,wrc)

