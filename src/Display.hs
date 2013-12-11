
module Display where

import Tile
import Config.Render
import Config.TileSet
import Util

import Graphics.Gloss hiding (Color)

-- TextureSet {{{

data TextureSet a = TextureSet
  { textureSet    :: TileSet (Picture,a)
  , textureSize   :: (Float,Float)
  , renderTexture :: Picture -> a -> Picture
  }

tileTexture :: Show a => TextureSet a -> TileIndex -> (Picture,a)
tileTexture = tsIndex . textureSet

mkTextureSet :: (Picture -> a -> Picture) -> TileSetConfig
  -> TileSet (Picture,a) -> TextureSet a
mkTextureSet rndr cfg ts = TextureSet
  { textureSet    = ts
  , textureSize   = onPair toEnum $ tileSize cfg
  , renderTexture = rndr
  }

mkTextureSet_ :: TileSetConfig -> TileSet Picture -> TextureSet ()
mkTextureSet_ cfg tp = TextureSet
  { textureSet  = tzip tp $ repeat ()
  , textureSize = onPair toEnum $ tileSize cfg
  , renderTexture = const
  }

-- }}}

displayTileMap :: Show a => RenderConfig -> TextureSet a -> Size -> TileMap -> IO ()
displayTileMap cfg ts sz tm = display mode (windowBackground cfg)
    $ scaleToWindow
    $ centerInWindow
    $ renderTileMap cfg ts tm
  where
  (h,v) = gridDimensions cfg ts sz
  res   = screenSize cfg
  --
  availRes = toEnum res - (2 * screenBorder cfg)
  sc = availRes / max h v
  scaleToWindow = scale sc sc
  --
  centerInWindow = translate (-(v/2)) (h/2)
  --
  mode = InWindow "gloss" (res,res) (0,0)

renderTileMap :: Show a => RenderConfig -> TextureSet a -> TileMap -> Picture
renderTileMap cfg ts tm = moveToOrigin
  . pictures
  . map ( render
        . fmap (tileTexture ts)
        )
  . tmAssocs
  $ tm
  where
  moveToOrigin = translate radX (-radY)
  render (c,(p,_)) = moveTileToCoord cfg ts c p
  (radX,radY) = onPair (/2) $ textureSize ts

moveTileToCoord :: RenderConfig -> TextureSet a -> Coord -> Picture -> Picture
moveTileToCoord rc ts (c,r) = translate (toEnum c * kx) (-(toEnum r * ky))
  where
  sp = tileSpacing rc
  (kx,ky) = onPair (+ sp) $ textureSize ts

gridDimensions :: RenderConfig -> TextureSet a -> Size -> (Float,Float)
gridDimensions cfg ts sz = (horiz,vert)
  where
  (c,r) = onPair toEnum sz
  horiz = (szX * c) + (sp * (c - 1))
  vert  = (szY * r) + (sp * (r - 1))
  (szX,szY) = textureSize ts
  sp = tileSpacing cfg

