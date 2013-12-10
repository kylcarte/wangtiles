{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet.Wang where

import Config.TileSet
import Tile.Wang
import Texture
import Util

loadWangTileSet :: TileSetConfig -> [Tile] -> IO WangTileSet
loadWangTileSet cfg ts = do
  texm <- loadTextureMap cfg
  return $ tzip texm ts

