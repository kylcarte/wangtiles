{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}

module Texture where

import Tile
import Util
import Config.TileSet

import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Traversable as T
import Codec.Picture
import Graphics.Gloss
import Graphics.Gloss.Juicy

loadTextureFromSets :: TileSets -> Text -> IO (TileSetConfig,TileSet Picture)
loadTextureFromSets tss n = do
  tsc <- lookupTileSetConfig n tss
  tm <- loadTextureMap tsc
  return (tsc,tm)

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

mkTextureMap :: M.Map (Int,Int) Picture
  -> TileSet (Int,Int)
  -> Maybe (TileSet Picture)
mkTextureMap = T.traverse . flip M.lookup

splitImage :: (Int,Int) -> DynamicImage -> [[DynamicImage]]
splitImage xy@(tileX,tileY) img
  | 0 <- imgX `mod` tileX
  , 0 <- imgY `mod` tileY
  = [ [ subImage (x,y) xy img
      | y <- [0,tileY..(imgY - 1)]
      ]
    | x <- [0,tileX..(imgX - 1)]
    ]
  | otherwise = error $
      "Texture of size " ++
      show imgX ++ "x" ++ show imgY ++
      " does not fit tiles of size " ++
      show tileX ++ "x" ++ show tileY
  where
  imgX = dynImgWidth  img
  imgY = dynImgHeight img

subImage :: (Int,Int) -> (Int,Int) -> DynamicImage -> DynamicImage
subImage (loX,loY) (w,h) = mapDynamicImage mkSubImage
  where
  mkSubImage i = generateImage (readSubImage i) w h
  readSubImage img x y = pixelAt img (loX + x) (loY + y)



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

dynImgWidth :: DynamicImage -> Int
dynImgWidth = onDynamicImage imageWidth

dynImgHeight :: DynamicImage -> Int
dynImgHeight = onDynamicImage imageHeight

