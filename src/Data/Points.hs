{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Points where

import Util

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Random
import Data.Aeson
import Data.Foldable (Foldable(..))
import Data.Maybe (mapMaybe)
import Linear
import qualified System.Random as R

-- Coord {{{

newtype Coord c = Coord
  { coord :: V2 c
  } deriving
    ( Eq , Ord , Show , Num , Fractional
    , Epsilon , R1 , R2
    , Functor , Applicative , Monad
    , Foldable , Traversable
    )

mkCoord :: c -> c -> Coord c
mkCoord = Coord .:. V2

instance R.Random c => R.Random (Coord c) where
  random      = runRandom $
        mkCoord
    <$> random
    <*> random
  randomR rng = runRandom $
        mkCoord
    <$> randomR (rng & both %~ view col)
    <*> randomR (rng & both %~ view row)

instance FromJSON c => FromJSON (Coord c) where
  parseJSON (Object o) =
        mkCoord 
    <$> o .: "row"
    <*> o .: "col"
  parseJSON _ = mzero

class IsCoord cd where
  col :: Lens' (cd c) c
  row :: Lens' (cd c) c

instance IsCoord Coord where
  col = _x
  row = _y

fromCoord :: (Coord c -> a) -> c -> c -> a
fromCoord f = f .:. mkCoord

toCoord :: (c -> c -> b) -> Coord c -> b
toCoord f c = f (c ^. col) (c ^. row)

-- Lenses, etc.

inGrid :: (Real c) => Size c -> Prism' (Coord c) (Coord c)
inGrid sz = prism' id $ \cd ->
  let (c,r) = cd & col   `view2` row
      (w,h) = sz & width `view2` height
  in
  if c >= 0 && r >= 0 && c < w && r < h
    then Just cd
    else Nothing

coordV2 :: Iso' (Coord c) (V2 c)
coordV2 = iso coord Coord

colRow :: Iso' (Coord c) (c,c)
colRow = iso (col `view2` row) (uncurry mkCoord)

onCoord :: (Real c) => (V2 c -> V2 c) -> Coord c -> Coord c
onCoord = under $ from coordV2

-- }}}

-- Size {{{

newtype Size c = Size
  { size :: V2 c
  } deriving
    ( Eq , Ord , Show , Num , Fractional
    , Epsilon , R1 , R2
    , Functor , Applicative , Monad
    , Foldable , Traversable
    )

mkSize :: c -> c -> Size c
mkSize = Size .:. V2

instance R.Random c => R.Random (Size c) where
  random      = runRandom $
        mkSize
    <$> random
    <*> random
  randomR rng = runRandom $
        mkSize
    <$> randomR (rng & both %~ view  width)
    <*> randomR (rng & both %~ view height)

instance FromJSON c => FromJSON (Size c) where
  parseJSON (Object o) =
        mkSize
    <$> o .: "width"
    <*> o .: "height"
  parseJSON _ = mzero

width :: Functor f => (c -> f c) -> Size c -> f (Size c)
width = _x

height :: Functor f => (c -> f c) -> Size c -> f (Size c)
height = _y

fromSize :: (Size c -> b) -> c -> c -> b
fromSize f = f .:. mkSize

toSize :: (c -> c -> b) -> Size c -> b
toSize f c = f (c ^. width) (c ^. height)

-- Lenses, etc.

sizeV2 :: (Real c) => Prism' (V2 c) (Size c)
sizeV2 = prism' size fromV2
  where
  fromV2 v = if allOf traverse (>= 0) v
    then Just $ Size v
    else Nothing

widthHeight :: Iso' (Size c) (c,c)
widthHeight = iso (width `view2` height) (uncurry mkSize)

onSize :: (Real c) => (V2 c -> V2 c) -> Size c -> Maybe (Size c)
onSize = underPrism sizeV2

-- }}}

-- Coord Enumeration {{{

coordInt :: (Integral c) => Size c -> Prism' Int (Coord c)
coordInt sz = prism' toInt fromInt
  where
  (w,h) = sz & width `view2` height
  toInt c = fromEnum $ w * c^.row + c^.col
  fromInt i = if r < w && c < h
    then Just $ mkCoord c r
    else Nothing
    where
    (r,c) = toEnum i `divMod` w

coordOnInt :: (Integral c) => Size c -> (Int -> Int)
  -> Coord c -> Maybe (Coord c)
coordOnInt sz = underPrism $ coordInt sz

enumCoords :: (Integral c) => Size c -> [Int] -> [Coord c]
enumCoords = mapMaybe . indexCoord

coordIndex :: (Integral c) => Size c -> Coord c -> Int
coordIndex = review . coordInt

indexCoord :: (Integral c) => Size c -> Int -> Maybe (Coord c)
indexCoord = preview . coordInt

lexMinBound :: Size c -> Int
lexMinBound _  = 0

lexMaxBound :: (Integral c) => Size c -> Int
lexMaxBound sz = coordIndex sz $ Coord $ size (sz - 1)

-- }}}

-- V2 Transformations {{{

projX :: Num a => V2 a -> V2 a
projX = _y .~ 0

projY :: Num a => V2 a -> V2 a
projY = _x .~ 0

reflX :: Num a => V2 a -> V2 a
reflX = _y %~ negate

reflY :: Num a => V2 a -> V2 a
reflY = _x %~ negate

reflId :: Num a => V2 a -> V2 a
reflId = (_x %~ negate) . perp

-- }}}

