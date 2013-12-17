
module Display.Wang where

import Config.Render
import Config.Render.Wang
import Config.TileSet
import Data.Points
import Data.TileMap
import Data.TileSet
import Display
import Texture
import Tile.Wang
import Util

import Control.Applicative
import Graphics.Gloss.Juicy

displayWangTileMap :: (Enum c) => WangRenderConfig
  -> WangTextureSet -> Size c -> TileMap c -> IO ()
displayWangTileMap = displayTileMap . wRenderConfig

mkWangTextureSet :: WangRenderConfig -> TileSetConfig
  -> TileSet Texture -> WangTileSet -> WangTextureSet
mkWangTextureSet cfg tsc = mkTextureSet (renderWangTile cfg rsz) tsc
  where
  rsz = toFloat <$> tileSize tsc

renderWangTile :: WangRenderConfig -> Size Float
  -> Texture -> Wang -> Picture
renderWangTile cfg rsz p t = pictures
  [ if wRenderEdges      cfg  then               es else blank
  , if tileRenderTexture cfg' then fromImageRGBA8 p else blank
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
    North -> moveV2 (        projY sz) . rotate 180
    South -> moveV2 (reflX $ projY sz)
    West  -> moveV2 (reflY $ projX sz) . rotate   90
    East  -> moveV2 (        projX sz) . rotate (-90)
