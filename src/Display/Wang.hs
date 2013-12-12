
module Display.Wang where

import Config.Render
import Config.Render.Wang
import Config.TileSet
import Display
import Tile
import Tile.Wang
import Util

import Graphics.Gloss hiding (Color)

type WangTextureSet = TextureSet Tile

displayWangTileMap :: WangRenderConfig -> WangTextureSet -> TileMap -> IO ()
displayWangTileMap = displayTileMap . wRenderConfig

mkWangTextureSet :: WangRenderConfig -> TileSetConfig
  -> WangTileSet -> WangTextureSet
mkWangTextureSet wrc tsc = mkTextureSet (renderWangTile wrc rsz) tsc
  where
  rsz = cast $ tileSize tsc

renderWangTile :: WangRenderConfig -> FSize
  -> Picture -> Tile -> Picture
renderWangTile cfg rsz p t = pictures
  [ if tileRenderTexture cfg' then p  else blank
  , if wRenderEdges      cfg  then es else blank
  ]
  where
  cfg' = wRenderConfig cfg
  es = pictures $ map (renderEdge cfg rsz) $ tileColors t

renderEdge :: WangRenderConfig -> FSize
  -> (Edge,Color) -> Picture
renderEdge cfg rsz (e,c) = moveToEdge $ colorEdge shape
  where
  shape = arcSolid 0 180
    $ wEdgeRadius cfg
    $ fToFSize edgeAxis rsz e
  colorEdge = color $ glossColor c
  moveToEdge = case e of
    North -> move (         fProjY rsz) . rotate 180
    South -> move (fReflX $ fProjY rsz)
    West  -> move (fReflY $ fProjX rsz) . rotate   90
    East  -> move (         fProjX rsz) . rotate (-90)
