{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Render.BackEnd.Gloss where

import Data.Points
import Render.BackEnd
import Util
import Util.HandleIO

import Control.Applicative
import Control.Lens
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Juicy (fromImageRGBA8)
import Linear

data GlossConfig = GlossConfig
  { glossScreenBorder     :: Float
  , glossScreenSize       :: Size Int
  , glossWindowBackground :: Gloss.Color
  , glossGridSize         :: Size Int
  , glossTextureSize      :: Size Float
  , glossTileSpacing      :: Size Float
  } deriving (Eq,Show)

mkDefaultGlossConfig :: Size Int -> GlossConfig
mkDefaultGlossConfig gs = GlossConfig
  { glossScreenBorder     = 20
  , glossScreenSize       = 600
  , glossWindowBackground = Gloss.black
  , glossGridSize         = gs
  , glossTextureSize      = 16
  , glossTileSpacing      = 0
  }

newtype GlossTexture = GlossTexture
  { glossTexture :: Gloss.Picture
  } deriving (Eq,Show)

instance BackEnd GlossConfig where
  type Picture GlossConfig = GlossTexture
  display cfg mp = do
    p <- io "Couldn't render texture" mp
    Gloss.display mode (glossWindowBackground cfg)
      $ scaleToWindow
      $ centerInWindow
      $ glossTexture p
    where
    gs, sp, sz, res, brd :: Size Float
    gs   = toFloat <$> glossGridSize cfg
    sp   = glossTileSpacing cfg
    sz   = glossTextureSize cfg * (gs + sp) - sp
    res' = glossScreenSize cfg
    res  = toFloat <$> res'
    brd  = square $ glossScreenBorder cfg
    --
    availRes :: Size Float
    availRes = res - (2 * brd)
    sc :: Size Float
    sc = availRes / square (foldV2 max sz)
    scaleToWindow :: Gloss.Picture -> Gloss.Picture
    scaleToWindow = scaleV2 sc
    --
    centerInWindow :: Gloss.Picture -> Gloss.Picture
    centerInWindow = moveV2 $ reflY $ sz / 2 -- (-(h/2)) (w/2)
    --
    mode = Gloss.InWindow "gloss" (res'^.widthHeight) (0,0)
  ----------------------------------------------------------
  renderTextureAt cfg cd =
      return
    . GlossTexture
    . moveToCoord cfg
      (fromEnum <$> cd)
    . fromImageRGBA8
  ----------------------------------------------------------
  blankPicture _ = GlossTexture Gloss.blank
  pictures     _ = GlossTexture . views (mapping glossPicture) Gloss.pictures


glossPicture :: Iso' GlossTexture Gloss.Picture
glossPicture = iso glossTexture GlossTexture

moveToCoord :: GlossConfig -> Coord Int -> Gloss.Picture -> Gloss.Picture
moveToCoord cfg c = moveV2 $ reflX $ cd * (ts + sp)
  where
  cd = view coordSize $ toFloat <$> c
  ts = glossTextureSize cfg
  sp = glossTileSpacing cfg

-- Helpers {{{

moveV2 :: (R2 f) => f Float -> Gloss.Picture -> Gloss.Picture
moveV2 = foldV2 Gloss.translate

scaleV2 :: (R2 f) => f Float -> Gloss.Picture -> Gloss.Picture
scaleV2 = foldV2 Gloss.scale

-- }}}

