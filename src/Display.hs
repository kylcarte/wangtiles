
module Display where

import qualified Data.Array as A
import qualified Data.Map as M
import Graphics.Gloss hiding (Color)
import qualified Graphics.Gloss as G

import Tile

displayTileMap :: RenderConfig -> TileMap -> IO ()
displayTileMap cfg tm = do
  display mode (windowBackground cfg)
    $ scaleToWindow
    $ centerInWindow
    $ renderTileMap cfg tm
  where
  (h,v) = tileMapDimensions cfg tm

  availRes = toEnum (screenSize cfg) - (2 * screenBorder cfg)
  sc = availRes / max h v
  scaleToWindow = scale sc sc

  dx = -(v/2)
  dy = h/2
  centerInWindow = translate dx dy

  res = screenSize cfg
  mode = InWindow "gloss" (res,res) (0,0)

renderTileMap :: RenderConfig -> TileMap -> Picture
renderTileMap cfg tm = moveToOrigin pic
  where
  ts = tileSet tm
  cis = gridContents $ tileMap tm
  ctps = map (fmap $ tileTexture ts) cis
  pic = pictures $ map render ctps
  moveToOrigin = translate (tileRadius cfg) (-(tileRadius cfg))
  render (c,tp) = moveTileToCoord cfg c $ renderTile cfg tp

moveTileToCoord :: RenderConfig -> Coord -> Picture -> Picture
moveTileToCoord cfg (r,c) = translate (toEnum c * k) (-(toEnum r * k))
  where
  k = (tileSize cfg) + (tileSpacing cfg)

renderTile :: RenderConfig -> (Tile,Picture) -> Picture
renderTile cfg (t,texture) = pictures
  [ if renderTexture cfg then texture else blank
  , if renderEdges   cfg then edges   else blank
  ]
  where
  edges = pictures $ map (renderEdge cfg) $ A.assocs $ tColors t

renderEdge :: RenderConfig -> (Edge,Color) -> Picture
renderEdge cfg (e,c) = moveToEdge $ colorEdge shape
  where
  shape = arcSolid 0 180 $ edgeRadius cfg
  -- rec = rectangleUpperSolid edgeWidth edgeWeight
  colorEdge = color $ case c of
    Red    -> red
    Green  -> green
    Yellow -> yellow
    Blue   -> blue
  moveToEdge = case e of
    North -> translate 0 (tileRadius cfg)    . rotate 180
    South -> translate 0 (-(tileRadius cfg))
    West  -> translate (-(tileRadius cfg)) 0 . rotate 90
    East  -> translate (tileRadius cfg)    0 . rotate (-90)

tileMapDimensions :: RenderConfig -> TileMap -> (Float,Float)
tileMapDimensions cfg tm = (horiz,vert)
  where
  (r,c) = gridSize $ tileMap tm
  r' = toEnum r
  c' = toEnum c
  horiz = (tileSize cfg * r') + (tileSpacing cfg * (r' - 1))
  vert  = (tileSize cfg * c') + (tileSpacing cfg * (c' - 1))

-- given a number of required textures, return a tile size (in pixels)
--   and a list of files from which to load the textures
type TextureTemplate = Int -> IO (Float,[FilePath])

loadDefaultTileSet :: TextureTemplate -> IO (TileSet,RenderConfig)
loadDefaultTileSet texTemplate = do
  (tSize,files) <- texTemplate $ length tiles
  textures <- mapM loadBMP files
  let ts = TileSet $
         M.fromList $ zip [ TileIndex i | i <- [0..] ] $
           zip tiles textures
  return (ts, defaultRenderConfig { tileSize = tSize })
  where
  tiles =
    [ mkTile Red   Green Blue   Yellow
    , mkTile Green Green Blue   Blue  
    , mkTile Red   Red   Yellow Yellow
    , mkTile Green Red   Yellow Blue  
    , mkTile Red   Green Yellow Blue  
    , mkTile Green Green Yellow Yellow
    , mkTile Red   Red   Blue   Blue  
    , mkTile Green Red   Blue   Yellow
    ]

data RenderConfig = RenderConfig
  { tileSize         :: Float
  , tileSpacing      :: Float
  , edgeRadius       :: Float
  , screenBorder     :: Float
  , screenSize       :: Int
  , tileBackground   :: G.Color
  , windowBackground :: G.Color
  , renderEdges      :: Bool
  , renderTexture    :: Bool
  } deriving (Eq,Show)

defaultRenderConfig :: RenderConfig
defaultRenderConfig = RenderConfig
  { tileSize         = 16
  , tileSpacing      = 0
  , edgeRadius       = 4
  , screenBorder     = 20
  , screenSize       = 600
  , tileBackground   = white
  , windowBackground = white
  , renderEdges      = False
  , renderTexture    = True
  }

tileRadius :: RenderConfig -> Float
tileRadius cfg = tileSize cfg / 2

