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

newtype Coord a = Coord
  { coord :: V2 a
  } deriving
    ( Eq , Ord , Show , Num , Fractional
    , Epsilon , R1 , R2
    , Functor , Applicative , Monad
    , Foldable , Traversable
    )

mkCoord :: a -> a -> Coord a
mkCoord = Coord .:. V2

instance R.Random a => R.Random (Coord a) where
  random      = runRandom $
        mkCoord
    <$> random
    <*> random
  randomR rng = runRandom $
        mkCoord
    <$> randomR (rng & both %~ view col)
    <*> randomR (rng & both %~ view row)

instance FromJSON a => FromJSON (Coord a) where
  parseJSON (Object o) =
        mkCoord 
    <$> o .: "row"
    <*> o .: "col"
  parseJSON _ = mzero

class IsCoord c where
  col :: Lens' (c a) a
  row :: Lens' (c a) a

instance IsCoord Coord where
  col = _x
  row = _y

fromCoord :: (Coord a -> b) -> a -> a -> b
fromCoord f = f .:. mkCoord

toCoord :: (a -> a -> b) -> Coord a -> b
toCoord f c = f (c ^. col) (c ^. row)

-- Lenses, etc.

inGrid :: (Integral a) => Size a -> Prism' (Coord a) (Coord a)
inGrid sz = prism' id $ \cd ->
  let (c,r) = cd & col   `view2` row
      (w,h) = sz & width `view2` height
  in
  if c >= 0 && r >= 0 && c < w && r < h
    then Just cd
    else Nothing

coordV2 :: Iso' (Coord a) (V2 a)
coordV2 = iso coord Coord

colRow :: Iso' (Coord a) (a,a)
colRow = iso (col `view2` row) (uncurry mkCoord)

-- }}}

-- Size {{{

newtype Size a = Size
  { size :: V2 a
  } deriving
    ( Eq , Ord , Show , Num , Fractional
    , Epsilon , R1 , R2
    , Functor , Applicative , Monad
    , Foldable , Traversable
    )

mkSize :: a -> a -> Size a
mkSize = Size .:. V2

instance R.Random a => R.Random (Size a) where
  random      = runRandom $
        mkSize
    <$> random
    <*> random
  randomR rng = runRandom $
        mkSize
    <$> randomR (rng & both %~ view  width)
    <*> randomR (rng & both %~ view height)

instance FromJSON a => FromJSON (Size a) where
  parseJSON (Object o) =
        mkSize
    <$> o .: "width"
    <*> o .: "height"
  parseJSON _ = mzero

width :: Functor f => (a -> f a) -> Size a -> f (Size a)
width = _x

height :: Functor f => (a -> f a) -> Size a -> f (Size a)
height = _y

fromSize :: (Size a -> b) -> a -> a -> b
fromSize f = f .:. mkSize

toSize :: (a -> a -> b) -> Size a -> b
toSize f c = f (c ^. width) (c ^. height)

-- Lenses, etc.

sizeV2 :: (Integral a) => Prism' (V2 a) (Size a)
sizeV2 = prism' size fromV2
  where
  fromV2 v = if allOf traverse (>= 0) v
    then Just $ Size v
    else Nothing

widthHeight :: Iso' (Size a) (a,a)
widthHeight = iso (width `view2` height) (uncurry mkSize)

-- }}}

-- Coord Enumeration {{{

coordInt :: (Integral a) => Size a -> Prism' Int (Coord a)
coordInt sz = prism' toInt fromInt
  where
  (w,h) = sz & width `view2` height
  toInt c = fromEnum $ w * c^.row + c^.col
  fromInt i = if r < w && c < h
    then Just $ mkCoord c r
    else Nothing
    where
    (r,c) = toEnum i `divMod` w

enumCoords :: (Integral c) => Size c -> [Int] -> [Coord c]
enumCoords = mapMaybe . indexCoord

coordIndex :: (Integral a) => Size a -> Coord a -> Int
coordIndex = review . coordInt

indexCoord :: (Integral a) => Size a -> Int -> Maybe (Coord a)
indexCoord = preview . coordInt

lexMinBound :: Size a -> Int
lexMinBound _  = 0

lexMaxBound :: (Integral a) => Size a -> Int
lexMaxBound sz = coordIndex sz $ Coord $ size (sz - 1)

-- }}}

-- V2 Transformations {{{

reflX :: Num a => V2 a -> V2 a
reflX = _y %~ negate

reflY :: Num a => V2 a -> V2 a
reflY = _x %~ negate

reflId :: Num a => V2 a -> V2 a
reflId = (_x %~ negate) . perp

-- }}}

