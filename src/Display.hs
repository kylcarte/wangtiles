{-# LANGUAGE ExistentialQuantification #-}

module Display where

import Data.Points
import Data.TileMap
import Data.TileSet
import Config.Render
import Config.TileSet
import Util

import Control.Applicative
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss hiding (Color, scale)
import Linear

-- TextureSet {{{

-- Instantiate to 'Size Float' ?
data TextureSet a = TextureSet
  { textureSet    :: TileSet (Picture,a)
  , textureSize   :: Size Float
  , renderTexture :: Picture -> a -> Picture
  }

tileTexture :: TextureSet a -> TileIndex -> (Picture,a)
tileTexture = tsIndex . textureSet

mkTextureSet :: (Picture -> a -> Picture) -> TileSetConfig
  -> TileSet (Picture,a) -> TextureSet a
mkTextureSet rndr cfg ts = TextureSet
  { textureSet  = ts
  , textureSize = toFloat <$> tileSize cfg
  , renderTexture = rndr
  }

mkTextureSet_ :: TileSetConfig -> TileSet Picture -> TextureSet ()
mkTextureSet_ cfg tp = TextureSet
  { textureSet  = tzip tp $ repeat ()
  , textureSize = (toFloat <$> tileSize cfg)
  , renderTexture = const
  }

-- }}}

displayPicture :: (Enum c, Real f, Enum f, Fractional f) =>
  RenderConfig -> Size c -> Size f -> Picture -> IO ()
displayPicture cfg gs ts =
  display mode (windowBackground cfg)
  . scaleToWindow
  . centerInWindow
  where
  sz :: V2 Float
  sz  = fmap toFloat $ size $ gridDimensions cfg gs ts
  res :: Int
  res = screenSize cfg
  --
  availRes :: Float
  availRes = toFloat res - (2 * screenBorder cfg)
  sc :: Float
  sc = availRes / toV2 max sz
  scaleToWindow :: Picture -> Picture
  scaleToWindow = scale $ pure sc
  --
  centerInWindow :: Picture -> Picture
  centerInWindow = move $ reflY $ sz / 2 -- (-(h/2)) (w/2)
  --
  mode = InWindow "gloss" (res,res) (0,0)

displayLayers :: (Enum c, Real f, Enum f, Fractional f) =>
  RenderConfig -> Size c -> Size f -> [Picture] -> IO ()
displayLayers cfg gs ts = displayPicture cfg gs ts . pictures

displayTileMap :: (Enum c) => RenderConfig
  -> TextureSet a -> Size c -> TileMap c -> IO ()
displayTileMap cfg txs sz tm = displayPicture cfg
  sz
  (textureSize txs)
  $ renderTileMap cfg txs tm

renderTileMap :: (Enum c) => RenderConfig
  -> TextureSet a -> TileMap c -> Picture
renderTileMap cfg ts tm =
    moveToOrigin
  . pictures
  . map (render . fmap (tileTexture ts))
  . tmAssocs
  $ tm
  where
  moveToOrigin = move $ reflX $ fmap toFloat $ size $ textureSize ts / 2
  render (c,(p,_)) = moveTileToCoord cfg ts c p

moveTileToCoord :: (Enum c) => RenderConfig
  -> TextureSet a -> Coord c -> Picture -> Picture
moveTileToCoord cfg ts cd = move $ reflX $ (coord $ toFloat <$> cd) * (sz + sp)
  where
  sz = size $ toFloat <$> textureSize ts
  sp = pure $ toFloat $ tileSpacing cfg

gridDimensions :: (Enum c, Real f, Enum f, Fractional f) =>
  RenderConfig -> Size c -> Size f -> Size f
gridDimensions cfg gs ts = sz * ts + sp * (sz - 1)
  where
  sz = fmap toFloat gs
  sp = pure $ toFloat $ tileSpacing cfg

move :: V2 Float -> Picture -> Picture
move = toV2 translate

scale :: V2 Float -> Picture -> Picture
scale = toV2 Gloss.scale

