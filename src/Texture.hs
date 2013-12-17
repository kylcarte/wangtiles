{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}

module Texture where

import Config.TileSet
import Data.Points
import Data.TileSet
import Util

import Control.Applicative
import Control.Lens
import Data.Word (Word8)
import qualified Data.Map as M
import qualified Data.Traversable as T
import Codec.Picture
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Juicy

type Texture = Image PixelRGBA8

loadTextureMap :: TileSetConfig -> IO (TileSet Texture)
loadTextureMap cfg = splitAndRender <$> loadTexture (textureFile cfg)
  where
  splitAndRender = 
      tsFromList
    . flattenAndIndex
    . deleteGridIndices
        (map (view colRow) $ ignoreTiles cfg)
    . splitImage (tileSize cfg)
  flattenAndIndex :: [[a]] -> [(Int,a)]
  flattenAndIndex = zip [0..] . concat

loadTexture :: FilePath -> IO Texture
loadTexture f = do
  ei <- readImage f
  img <- either fail return ei
  case img of
    ImageRGBA8 i -> return i
    _ -> fail $ "Image format is currently unsupported: " ++
           dynamicImageVariety img

mkTextureMap :: (Ord c) => M.Map (Coord c) Texture
  -> TileSet (Coord c) -> Maybe (TileSet Texture)
mkTextureMap = T.traverse . flip M.lookup

splitImage :: Size Int -> Texture -> [[Texture]]
splitImage ts img
  | 0 <- onV2 mod imgSize ts
  = map (map $ subImage ts img)
    $ coordGrid imgSize (Just ts)
  | otherwise = error $
      "Texture of size " ++ show imgSize ++
      " does not fit tiles of size " ++ show ts
  where
  imgSize       = dynamicImageSize img

subImage :: (Enum c) => Size c -> Texture -> Coord c -> Texture
subImage sz img cd = mkSubImage cd sz img

mkSubImage :: (Enum c, Pixel a) => Coord c -> Size c -> Image a -> Image a
mkSubImage cd sz i =
    flip toSize s
  . generateImage
  . pixelFromCoord i
  $ c
  where
  c = fmap fromEnum cd
  s = fmap fromEnum sz

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

type RGBA = (Word8,Word8,Word8,Word8)

dynamicImageSize :: Texture -> Size Int
dynamicImageSize img = mkSize (imageWidth img) (imageHeight img)

solidColor :: Pixel a => Size Int -> a -> Image a
solidColor sz c = foldV2 (generateImage $ \_x _y -> c) sz

solidRGBA8 :: Size Int -> RGBA -> Texture
solidRGBA8 sz = solidColor sz . mkRGBA8

shadeImage :: Rational -> RGBA -> Image PixelRGBA8 -> Image PixelRGBA8
shadeImage rat c = pixelMap $ \c2 -> mixColors rat c1 c2
  where
  c1 = mkRGBA8 c

rgba8 :: PixelRGBA8 -> RGBA
rgba8 (PixelRGBA8 r g b a) = (r,g,b,a)

mkRGBA8 :: RGBA -> PixelRGBA8
mkRGBA8 = uncurry4 PixelRGBA8

mixColors :: Rational -> PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
mixColors rat c1 c2 = PixelRGBA8
  (enums $ rat * r1 + inv * r2)
  (enums $ rat * g1 + inv * g2)
  (enums $ rat * b1 + inv * b2)
  (enums $ rat * a1 + inv * a2)
  where
  (r1,g1,b1,a1) = on4 toRational $ rgba8 c1
  (r2,g2,b2,a2) = on4 toRational $ rgba8 c2
  inv           = 1 - rat


-- }}}

