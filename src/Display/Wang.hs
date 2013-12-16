
module Display.Wang where

import Config.Render
import Config.Render.Wang
import Config.TileSet
import Data.Points
import Display
import Tile
import Tile.Wang
import Util

import Control.Applicative
import Graphics.Gloss hiding (Color)

displayWangTileMap :: (Enum c) =>
  WangRenderConfig -> WangTextureSet -> TileMap c -> IO ()
displayWangTileMap = displayTileMap . wRenderConfig

mkWangTextureSet :: WangRenderConfig -> TileSetConfig
  -> WangTileSet -> WangTextureSet
mkWangTextureSet cfg tsc = mkTextureSet (renderWangTile cfg rsz) tsc
  where
  rsz = toFloat <$> tileSize tsc

renderWangTile :: WangRenderConfig -> Size Float
  -> Picture -> Tile -> Picture
renderWangTile cfg rsz p t = pictures
  [ if wRenderEdges      cfg  then es else blank
  , if tileRenderTexture cfg' then p  else blank
  ]
  where
  cfg' = wRenderConfig cfg
  es = pictures $ map (renderEdge cfg rsz) $ tileColors t

renderEdge :: WangRenderConfig -> Size Float
  -> (Edge,Color) -> Picture
renderEdge cfg rsz (e,c) = moveToEdge $ colorEdge shape
  where
  sz = size rsz
  shape = arcSolid 0 180
    $ wEdgeRadius cfg
    $ toSize edgeAxis rsz e
  colorEdge = color $ glossColor c
  moveToEdge = case e of
    North -> move (        projY sz) . rotate 180
    South -> move (reflX $ projY sz)
    West  -> move (reflY $ projX sz) . rotate   90
    East  -> move (        projX sz) . rotate (-90)
