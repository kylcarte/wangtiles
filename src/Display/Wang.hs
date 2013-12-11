
module Display.Wang where

import Tile
import Tile.Wang
import Config.Render
import Config.Render.Wang
import Config.TileSet
import Display
import Util

import Graphics.Gloss hiding (Color)

type WangTextureSet = TextureSet Tile

displayWangTileMap :: WangRenderConfig -> WangTextureSet -> Size -> TileMap -> IO ()
displayWangTileMap = displayTileMap . wRenderConfig

mkWangTextureSet :: WangRenderConfig -> TileSetConfig
  -> WangTileSet -> WangTextureSet
mkWangTextureSet wrc tsc = mkTextureSet (renderWangTile wrc rxy) tsc
  where
  rxy = onPair toEnum $ tileSize tsc

renderWangTile :: WangRenderConfig -> (Float,Float)
  -> Picture -> Tile -> Picture
renderWangTile cfg rxy p t = pictures
  [ if tileRenderTexture cfg' then p  else blank
  , if wRenderEdges      cfg  then es else blank
  ]
  where
  cfg' = wRenderConfig cfg
  es = pictures $ map (renderEdge cfg rxy) $ tileColors t

renderEdge :: WangRenderConfig -> (Float,Float)
  -> (Edge,Color) -> Picture
renderEdge cfg (radX,radY) (e,c) = moveToEdge $ colorEdge shape
  where
  shape = arcSolid 0 180 $ wEdgeRadius cfg $ edgeAxis radX radY e
  colorEdge = color $ glossColor c
  moveToEdge = case e of
    North -> translate 0 radY    . rotate 180
    South -> translate 0 (-radY)
    West  -> translate (-radX) 0 . rotate 90
    East  -> translate radX    0 . rotate (-90)

