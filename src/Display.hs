{-# LANGUAGE ExistentialQuantification #-}

module Display where

import Tile
import Config.Render
import Config.TileSet
import Util

import Graphics.Gloss hiding (Color)

-- TextureSet {{{

data TextureSet a = TextureSet
  { textureSet    :: TileSet (Picture,a)
  , textureSize   :: FSize
  , renderTexture :: Picture -> a -> Picture
  }

tileTexture :: TextureSet a -> TileIndex -> (Picture,a)
tileTexture = tsIndex . textureSet

mkTextureSet :: (Picture -> a -> Picture) -> TileSetConfig
  -> TileSet (Picture,a) -> TextureSet a
mkTextureSet rndr cfg ts = TextureSet
  { textureSet    = ts
  , textureSize   = cast $ tileSize cfg
  , renderTexture = rndr
  }

mkTextureSet_ :: TileSetConfig -> TileSet Picture -> TextureSet ()
mkTextureSet_ cfg tp = TextureSet
  { textureSet  = tzip tp $ repeat ()
  , textureSize = cast $ tileSize cfg
  , renderTexture = const
  }

-- }}}

displayPicture :: RenderConfig -> Size -> FSize -> Picture -> IO ()
displayPicture cfg gs ts p = display mode (windowBackground cfg)
  . scaleToWindow
  . centerInWindow
  $ p
  where
  sz = gridDimensions cfg gs ts
  res   = screenSize cfg
  --
  availRes = toEnum res - (2 * screenBorder cfg)
  sc = availRes / fToFSize max sz
  scaleToWindow = scale sc sc
  --
  centerInWindow = move $ fReflY $ sz / 2 -- (-(h/2)) (w/2)
  --
  mode = InWindow "gloss" (res,res) (0,0)

displayLayers :: RenderConfig -> Size -> FSize -> [Picture] -> IO ()
displayLayers cfg gs ts = displayPicture cfg gs ts . pictures

displayTileMap :: RenderConfig -> TextureSet a -> TileMap -> IO ()
displayTileMap cfg txs tm = displayPicture cfg
  (tmSize tm)
  (textureSize txs)
  $ renderTileMap cfg txs tm

renderTileMap :: RenderConfig -> TextureSet a -> TileMap -> Picture
renderTileMap cfg ts tm = moveToOrigin
  . pictures
  . map (render . fmap (tileTexture ts))
  . tmAssocs
  $ tm
  where
  moveToOrigin = move $ fReflX $ textureSize ts / 2
  render (c,(p,_)) = moveTileToCoord cfg ts c p

moveTileToCoord :: RenderConfig -> TextureSet a -> Coord -> Picture -> Picture
moveTileToCoord cfg ts cd = move $ fReflX $ cast cd * (sz + enum2 sp)
  where
  sz = textureSize ts
  sp = tileSpacing cfg

gridDimensions :: RenderConfig -> Size -> FSize -> FSize
gridDimensions cfg gs ts = sz * ts + sp * (sz - 1)
  where
  sz = cast gs
  sp = enum2 $ tileSpacing cfg

move :: FSize -> Picture -> Picture
move = fToFSize translate

