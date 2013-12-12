{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}

module Texture where

import Tile
import Util
import Config.TileSet

import Control.Arrow ((&&&))
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Traversable as T
import Codec.Picture
import Graphics.Gloss
import Graphics.Gloss.Juicy

loadTextureMap :: TileSetConfig -> IO (TileSet Picture)
loadTextureMap cfg = readImage (textureFile cfg) >>= splitAndRender
  where
  splitAndRender = either fail $ return
    . tsFromList
    . flattenAndIndex
    . map (map fromDynamicImage)
    . deleteGridIndices (ignoreTiles cfg)
    . splitImage (tileSize cfg)
  flattenAndIndex :: [[Maybe a]] -> [(Int,a)]
  flattenAndIndex = zip [0..] . catMaybes . concat

mkTextureMap :: M.Map Coord Picture
  -> TileSet Coord
  -> Maybe (TileSet Picture)
mkTextureMap = T.traverse . flip M.lookup

splitImage :: Size -> DynamicImage -> [[DynamicImage]]
splitImage ts img
  | 0 <- imgX `mod` tileX
  , 0 <- imgY `mod` tileY
  = [ [ subImage (Coord { col = x , row = y }) ts img
      | x <- [0,tileX..(imgX - 1)]
      ]
    | y <- [0,tileY..(imgY - 1)]
    ]
  | otherwise = error $
      "Texture of size " ++
      show imgX ++ "x" ++ show imgY ++
      " does not fit tiles of size " ++
      show tileX ++ "x" ++ show tileY
  where
  (tileX,tileY) = widthHeight ts
  (imgX,imgY) = diWidthHeight img

subImage :: Coord -> Size -> DynamicImage -> DynamicImage
subImage cd sz = mapDynamicImage $ mkSubImage cd sz

mkSubImage :: Pixel a => Coord -> Size -> Image a -> Image a
mkSubImage cd sz i = flip fToSize sz
  $ generateImage
  $ pixelFromCoord i cd

pixelFromCoord :: Pixel a => Image a -> Coord -> Int -> Int -> a
pixelFromCoord i cd = fFromCoord $ fToCoord (pixelAt i) . (cd +)

-- DynamicImage {{{

-- ugh
onDynamicImage :: (forall a. Pixel a => Image a -> b)
  -> DynamicImage -> b
onDynamicImage f img = case img of
  ImageY8     i -> f i
  ImageY16    i -> f i
  ImageYF     i -> f i
  ImageYA8    i -> f i
  ImageYA16   i -> f i
  ImageRGB8   i -> f i
  ImageRGB16  i -> f i
  ImageRGBF   i -> f i
  ImageRGBA8  i -> f i
  ImageRGBA16 i -> f i
  ImageYCbCr8 i -> f i
  ImageCMYK8  i -> f i
  ImageCMYK16 i -> f i

mapDynamicImage :: (forall a. Pixel a => Image a -> Image a)
  -> DynamicImage -> DynamicImage
mapDynamicImage f img = case img of
  ImageY8     i -> ImageY8     $ f i
  ImageY16    i -> ImageY16    $ f i
  ImageYF     i -> ImageYF     $ f i
  ImageYA8    i -> ImageYA8    $ f i
  ImageYA16   i -> ImageYA16   $ f i
  ImageRGB8   i -> ImageRGB8   $ f i
  ImageRGB16  i -> ImageRGB16  $ f i
  ImageRGBF   i -> ImageRGBF   $ f i
  ImageRGBA8  i -> ImageRGBA8  $ f i
  ImageRGBA16 i -> ImageRGBA16 $ f i
  ImageYCbCr8 i -> ImageYCbCr8 $ f i
  ImageCMYK8  i -> ImageCMYK8  $ f i
  ImageCMYK16 i -> ImageCMYK16 $ f i

diWidthHeight :: DynamicImage -> (Int,Int)
diWidthHeight = onDynamicImage imageWidth &&& onDynamicImage imageHeight

-- }}}

