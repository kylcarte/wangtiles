
module Display where

import qualified Data.Array as A
import Graphics.Gloss hiding (Color(..))
import qualified Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Display hiding (Color(..))

import Tile

displayTileMap :: Int -> TileMap -> IO ()
displayTileMap res tm = do
  display mode black
    $ scaleToWindow
    $ centerInWindow
    $ renderTileMap tm
  where
  (h,v) = tileMapDimensions tm

  availRes = toEnum res - (2 * screenBorder)
  sc = availRes / max h v
  scaleToWindow = scale sc sc

  dx = -(v/2)
  dy = h/2
  centerInWindow = translate dx dy

  mode = InWindow "gloss" (res,res) (0,0)

renderTileMap :: TileMap -> Picture
renderTileMap tm = moveToOrigin pic
  where
  ts = tileSet tm
  cis = gridContents $ tileMap tm
  cts = map (fmap $ tileIn ts) cis
  pic = pictures $ map (uncurry renderTile) cts
  moveToOrigin = translate tileRadius (-tileRadius)

renderTile :: Coord -> Tile -> Picture
renderTile (r,c) (Tile cs) = moveToCoord
  $ pictures
    [ bg
    , edges
    ]
  where
  bg = color tileBackground $ rectangleSolid tileSize tileSize
  border = color black $ rectangleWire tileSize tileSize
  edges = pictures $ map renderEdge $ A.assocs cs

  k = tileSize + tileSpacing
  moveToCoord = translate (toEnum c * k) (-(toEnum r * k))

renderEdge :: (Edge,Color) -> Picture
renderEdge (e,c) = moveToEdge $ colorRec rec
  where
  rec = rectangleUpperSolid edgeWidth edgeWeight
  colorRec = color $ case c of
    Red    -> red
    Green  -> green
    Yellow -> yellow
    Blue   -> blue
  moveToEdge = case e of
    North -> translate 0 tileRadius    . rotate 180
    South -> translate 0 (-tileRadius)
    West  -> translate (-tileRadius) 0 . rotate 90
    East  -> translate tileRadius    0 . rotate (-90)

tileMapDimensions :: TileMap -> (Float,Float)
tileMapDimensions tm = (horiz,vert)
  where
  (r,c) = gridSize $ tileMap tm
  r' = toEnum r
  c' = toEnum c
  horiz = (tileSize * r') + (tileSpacing * (r' - 1))
  vert  = (tileSize * c') + (tileSpacing * (c' - 1))

tileRadius :: Float
tileRadius = 20

tileSpacing :: Float
tileSpacing = 0

edgeWeight :: Float
edgeWeight = 4

screenBorder :: Float
screenBorder = 20

tileSize :: Float
tileSize = 2 * tileRadius

edgeWidth :: Float
edgeWidth = tileSize - (2 * edgeWeight + 4)

tileBackground :: G.Color
tileBackground = white

