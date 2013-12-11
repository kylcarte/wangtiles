
module Display where

import Tile
import Config.Render
import Config.TileSet
import Util

import Data.Maybe (fromMaybe)
import Graphics.Gloss hiding (Color)

-- TextureSet {{{

data TextureSet a = TextureSet
  { textureSet    :: TileSet (Picture,a)
  , textureSize   :: (Float,Float)
  , renderTexture :: Picture -> a -> Picture
  }

tileTexture :: TextureSet a -> TileIndex -> (Picture,a)
tileTexture = tsIndex . textureSet

mkTextureSet :: (Picture -> a -> Picture) -> TileSetConfig
  -> TileSet Picture -> TileSet a -> TextureSet a
mkTextureSet rndr cfg tp ta = TextureSet
  { textureSet  = fromMaybe err $ tsZipR tp ta
  , textureSize = onPair toEnum $ tileSize cfg
  , renderTexture = rndr
  }
  where
  err = error "TileSet index mismatch"

mkTextureSet_ :: TileSetConfig -> TileSet Picture -> TextureSet ()
mkTextureSet_ cfg tp = TextureSet
  { textureSet  = tzip tp $ repeat ()
  , textureSize = onPair toEnum $ tileSize cfg
  , renderTexture = const
  }

-- }}}

displayTileMap :: RenderConfig -> TextureSet a -> TileMap -> IO ()
displayTileMap cfg ts tm = display mode (windowBackground cfg)
    $ scaleToWindow
    $ centerInWindow
    $ renderTileMap cfg ts tm
  where
  (h,v) = tileMapDimensions cfg ts tm
  res   = screenSize cfg
  --
  availRes = toEnum res - (2 * screenBorder cfg)
  sc = availRes / max h v
  scaleToWindow = scale sc sc
  --
  centerInWindow = translate (-(v/2)) (h/2)
  --
  mode = InWindow "gloss" (res,res) (0,0)

renderTileMap :: RenderConfig -> TextureSet a -> TileMap -> Picture
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

tileMapDimensions :: RenderConfig -> TextureSet a -> TileMap -> (Float,Float)
tileMapDimensions cfg ts tm = (horiz,vert)
  where
  (c,r) = onPair toEnum $ tmSize tm
  horiz = (szX * c) + (sp * (c - 1))
  vert  = (szY * r) + (sp * (r - 1))
  (szX,szY) = textureSize ts
  sp = tileSpacing cfg

