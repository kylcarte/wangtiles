{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}

module Texture
  ( Texture
  , Textures
  , loadTextures
  , loadTexturesFrom
  , TextureConfig (..)
  , StdConfig (..)
  ) where

import Data.Points
import Data.TileSet
import Texture.Config
import Util.HandleIO

import Control.Applicative
import Data.List ((\\))
import qualified Data.Map as M
import Data.Text (Text)
import Codec.Picture

type Textures = TileSet Texture

type Texture = Image PixelRGBA8

loadTexturesFrom :: TextureConfig c => TextureConfigs c -> Text -> IO (TileSet Texture)
loadTexturesFrom cfgs nm = do
  cfg <- io' $ M.lookup nm cfgs
  loadTextures cfg

loadTextures :: TextureConfig c => c -> IO (TileSet Texture)
loadTextures cfg = splitAndRender <$> loadTexture (textureFile cfg)
  where
  splitAndRender = 
      tsFromOrderedList
    . splitImage (ignoreTiles cfg) (tileSize cfg)

loadTexture :: FilePath -> IO Texture
loadTexture f = do
  ei <- readImage f
  img <- either fail return ei
  case img of
    ImageRGBA8 i -> return i
    _ -> fail $ "Image format \"" ++
           dynamicImageVariety img ++
           "\" is currently unsupported. \
           \Please use 8bit RGBA png."

splitImage :: [Coord Int] -> Size Int -> Texture -> [Texture]
splitImage ign ts img
  | 0 <- zipWithV2 mod imgSize ts
  = map (subImage ts img) $
    concat (coordGridChunks ts imgSize) \\ ign
  | otherwise = error $
      "Texture of size " ++ show imgSize ++
      " does not fit tiles of size " ++ show ts
  where
  imgSize       = dynamicImageSize img

subImage :: (Pixel a) => Size Int -> Image a -> Coord Int -> Image a
subImage sz img cd = foldV2 (generateImage $ pixelFromCoord img cd) sz

pixelFromCoord :: Pixel a => Image a -> Coord Int -> Int -> Int -> a
pixelFromCoord i cd = fromCoord $ toCoord (pixelAt i) . (cd +)

-- Texture {{{

dynamicImageVariety :: DynamicImage -> String
dynamicImageVariety img = case img of
  ImageY8     _ -> "ImageY8"
  ImageY16    _ -> "ImageY16"
  ImageYF     _ -> "ImageYF"
  ImageYA8    _ -> "ImageYA8"
  ImageYA16   _ -> "ImageYA16"
  ImageRGB8   _ -> "ImageRGB8"
  ImageRGB16  _ -> "ImageRGB16"
  ImageRGBF   _ -> "ImageRGBF"
  ImageRGBA8  _ -> "ImageRGBA8"
  ImageRGBA16 _ -> "ImageRGBA16"
  ImageYCbCr8 _ -> "ImageYCbCr8"
  ImageCMYK8  _ -> "ImageCMYK8"
  ImageCMYK16 _ -> "ImageCMYK16"

dynamicImageSize :: Texture -> Size Int
dynamicImageSize img = mkSize (imageWidth img) (imageHeight img)


-- }}}

