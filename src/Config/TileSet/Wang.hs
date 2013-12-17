{-# LANGUAGE OverloadedStrings #-}

module Config.TileSet.Wang where

import Config.Render.Wang
import Config.TileSet
import Data.TileSet
import Display.Wang
import Tile.Wang
import Texture

import Data.Text (Text)

loadWangTextureSet :: WangRenderConfig -> TileSets -> Text
  -> TileSet Wang -> IO WangTextureSet
loadWangTextureSet wrc tss n ts = do
  cfg <- lookupTileSetConfig tss n
  txm <- loadTextureMap cfg
  -- wts <- io errMsg $ tsZipR txm ts
  return $ mkWangTextureSet wrc cfg txm ts
  -- where
  -- errMsg = "Texture provided by TileSetConfig does not \
  --            \contain sufficient textures for Wang TileSet"

