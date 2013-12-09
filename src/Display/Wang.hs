
module Display.Wang where

import qualified Data.Array as A

import Tile
import Tile.Wang
import Config.Render
import Config.Render.Wang
import Graphics.Gloss hiding (Color)

displayTileMap :: WangRenderConfig -> WangTileSet -> TileMap -> IO ()
displayTileMap cfg ts tm = do
  display mode (windowBackground cfg')
    $ scaleToWindow
    $ centerInWindow
    $ renderTileMap cfg ts tm
  where
  cfg' = wRenderConfig cfg
  (h,v) = tileMapDimensions cfg tm

  availRes = toEnum (screenSize cfg') - (2 * screenBorder cfg')
  sc = availRes / max h v
  scaleToWindow = scale sc sc

  dx = -(v/2)
  dy = h/2
  centerInWindow = translate dx dy

  res = screenSize cfg'
  mode = InWindow "gloss" (res,res) (0,0)

renderTileMap :: WangRenderConfig -> WangTileSet -> TileMap -> Picture
renderTileMap cfg ts tm = moveToOrigin pic
  where
  cis = gridContents $ tileMap tm
  ctps = map (fmap $ tileTexture ts) cis
  pic = pictures $ map render ctps
  moveToOrigin = translate radX (-radY)
  render (c,pt) = moveTileToCoord cfg c $ renderTile cfg pt
  (radX,radY) = wTileRadius cfg

moveTileToCoord :: WangRenderConfig -> Coord -> Picture -> Picture
moveTileToCoord cfg (r,c) = translate (toEnum c * kx) (-(toEnum r * ky))
  where
  cfg' = wRenderConfig cfg
  sp = tileSpacing cfg'
  kx = szX + sp
  ky = szY + sp
  (szX,szY) = wTileSize cfg

renderTile :: WangRenderConfig -> (Picture,Tile) -> Picture
renderTile cfg (tex,tile) = pictures
  [ if renderTexture cfg' then tex   else blank
  , if wRenderEdges  cfg  then edges else blank
  ]
  where
  cfg' = wRenderConfig cfg
  edges = pictures $ map (renderEdge cfg) $ A.assocs $ tColors tile

renderEdge :: WangRenderConfig -> (Edge,Color) -> Picture
renderEdge cfg (e,c) = moveToEdge $ colorEdge shape
  where
  shape = arcSolid 0 180 $ wEdgeRadius cfg edge
  edge = case e of
    North -> radX
    South -> radX
    West  -> radY
    East  -> radY
  colorEdge = color $ case c of
    Red    -> red
    Green  -> green
    Yellow -> yellow
    Blue   -> blue
  moveToEdge = case e of
    North -> translate 0 radY    . rotate 180
    South -> translate 0 (-radY)
    West  -> translate (-radX) 0 . rotate 90
    East  -> translate radX    0 . rotate (-90)
  (radX,radY) = wTileRadius cfg

tileMapDimensions :: WangRenderConfig -> TileMap -> (Float,Float)
tileMapDimensions cfg tm = (horiz,vert)
  where
  cfg' = wRenderConfig cfg
  (r,c) = gridSize $ tileMap tm
  r' = toEnum r
  c' = toEnum c
  horiz = (szX * r') + (sp * (r' - 1))
  vert  = (szY * c') + (sp * (c' - 1))
  (szX,szY) = wTileSize cfg
  sp = tileSpacing cfg'

