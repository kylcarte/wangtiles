
module Texture.Util where

import Data.Points
import Texture
import Util

import Codec.Picture
import Data.Word (Word8)

-- Image Generation {{{

solidRGBA8 :: Size Int -> RGBA -> Texture
solidRGBA8 sz = solidColor sz . mkRGBA8

solidColor :: Pixel a => Size Int -> a -> Image a
solidColor sz c = foldV2 (generateImage $ \_x _y -> c) sz

-- }}}

-- Color Manipulation {{{

type RGBA = (Word8,Word8,Word8,Word8)

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

