{-# LANGUAGE ExistentialQuantification #-}

module Display
  ( module Display
  , module Graphics.Gloss.Data.Picture
  ) where

import Data.Points
import Data.TileMap
import Data.TileSet
import Config.Render
import Config.TileSet
import Util

import Control.Applicative
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Linear

-- TextureSet {{{

-- Instantiate to 'Size Float' ?
data TextureSet a = TextureSet
  { textureSet    :: TileSet (Picture,a)
  , textureSize   :: Size Float
  , renderTexture :: Picture -> a -> Picture
  }

tileTexture :: TextureSet a -> TileIndex -> (Picture,a)
tileTexture ts = tsIndex (textureSet ts)

mkTextureSet :: (Picture -> a -> Picture) -> TileSetConfig
  -> TileSet Picture -> TileSet a -> TextureSet a
mkTextureSet rndr cfg tp ts = fromMaybe err $ TextureSet
  <$> tsZipR tp ts
  <*> pure (toFloat <$> tileSize cfg)
  <*> pure rndr
  where
  err = error "Insufficient textures for TileSet"

{-
mkTextureSet_ :: TileSetConfig -> TextureSet ()
mkTextureSet_ cfg = TextureSet
  { textureSet  = tzip tp $ repeat ()
  , textureSize = (toFloat <$> tileSize cfg)
  , renderTexture = const
  }
-}

-- }}}

displayPicture :: (Enum c, Real f, Enum f, Fractional f)
  => RenderConfig -> Size c -> Size f -> Picture -> IO ()
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
  scaleToWindow = scaleV2 $ pure sc
  --
  centerInWindow :: Picture -> Picture
  centerInWindow = moveV2 $ reflY $ sz / 2 -- (-(h/2)) (w/2)
  --
  mode = InWindow "gloss" (res,res) (0,0)

displayLayers :: (Enum c, Real f, Enum f, Fractional f)
  => RenderConfig -> Size c -> Size f -> [Picture] -> IO ()
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
  moveToOrigin = moveV2 $ reflX $ fmap toFloat $ size $ textureSize ts / 2
  render (c,(p,a)) = moveTileToCoord cfg ts c $ renderTexture ts p a

moveTileToCoord :: (Enum c) => RenderConfig
  -> TextureSet a -> Coord c -> Picture -> Picture
moveTileToCoord cfg ts cd = moveV2 $ reflX $ (coord $ toFloat <$> cd) * (sz + sp)
  where
  sz = size $ toFloat <$> textureSize ts
  sp = pure $ toFloat $ tileSpacing cfg

gridDimensions :: (Enum c, Real f, Enum f, Fractional f)
  => RenderConfig -> Size c -> Size f -> Size f
gridDimensions cfg gs ts = sz * ts + sp * (sz - 1)
  where
  sz = fmap toFloat gs
  sp = pure $ toFloat $ tileSpacing cfg

moveV2 :: V2 Float -> Picture -> Picture
moveV2 = toV2 translate

scaleV2 :: V2 Float -> Picture -> Picture
scaleV2 = toV2 scale

