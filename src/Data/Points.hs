{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Points where

import Util

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Foldable (Foldable(..))
import Data.Ix
import Data.List (elemIndex)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid
import Linear hiding (diagonal,perp)
import qualified System.Random as R

-- Coord {{{

newtype Coord c = Coord
  { coord :: V2 c
  } deriving
    ( Eq, Show, Num, Fractional
    , Epsilon, R1, R2, Additive
    , Functor, Applicative, Monad
    , Foldable, Traversable
    )

instance (Ord c) => Ord (Coord c) where
  compare c1 c2 = (c1 `cRows` c2) <> (c1 `cCols` c2)
    where
    cCols = compare `on` view col
    cRows = compare `on` view row

instance (CoordType c, Ix c) => Ix (Coord c) where
  range (l,h)     = concat $ coordGridAt l (view coordSize $ h - l)
  index lh c
    | inRange lh c
    , Just i <- elemIndex c $ range lh
    = i
    | otherwise
    = error $ "Coord " ++ show c ++ " is not in range " ++ show lh
  inRange (l,h) c = l^.col <= x && x <= h^.col &&
                    l^.row <= y && y <= h^.row
    where
    x = c^.col
    y = c^.row

mkCoord :: c -> c -> Coord c
mkCoord = Coord .:. V2

instance R.Random c => R.Random (Coord c) where
  random g0 = (mkCoord x y,g2)
    where
    (x,g1) = R.random g0
    (y,g2) = R.random g1
  randomR rng g0 = (mkCoord x y,g2)
    where
    (x,g1) = R.randomR (rng & both %~ view col) g0
    (y,g2) = R.randomR (rng & both %~ view row) g1

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

onCoord :: (CoordType c) => (V2 c -> V2 c) -> Coord c -> Coord c
onCoord = under $ from coordV2

sizeFromBounds :: (CoordType c) => (Coord c,Coord c) -> (Coord c,Size c)
sizeFromBounds (l,h) = (l,view coordSize $ h - l)

-- }}}

-- Coord Classes {{{

class (Ord c, Show c, Num c, Enum c) => CoordType c where
instance CoordType Int where
instance CoordType Integer where
instance CoordType Float where
instance CoordType Double where

class (CoordType c, Integral c) => DiscreteCoord c where
instance (CoordType c, Integral c) => DiscreteCoord c where

class (CoordType c, Fractional c) => ContCoord c where
instance (CoordType c, Fractional c) => ContCoord c where

diagonal :: c -> Coord c
diagonal c = mkCoord c c

fromCoord :: (Coord c -> a) -> c -> c -> a
fromCoord f = f .:. mkCoord

toCoord :: (c -> c -> b) -> Coord c -> b
toCoord f c = f (c ^. col) (c ^. row)

-- }}}

-- Size {{{

newtype Size c = Size
  { size :: V2 c
  } deriving
    ( Eq, Ord, Show, Num, Fractional
    , Epsilon, R1, R2, Additive
    , Functor, Applicative, Monad
    , Foldable, Traversable
    )

mkSize :: c -> c -> Size c
mkSize = Size .:. V2

instance R.Random c => R.Random (Size c) where
  random g0 = (mkSize w h,g2)
    where
    (w,g1) = R.random g0
    (h,g2) = R.random g1
  randomR rng g0 = (mkSize w h,g2)
    where
    (w,g1) = R.randomR (rng & both %~ view  width) g0
    (h,g2) = R.randomR (rng & both %~ view height) g1

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

square :: c -> Size c
square c = mkSize c c

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

coordInt :: (DiscreteCoord c) => Size c -> Prism' Int (Coord c)
coordInt sz = prism' toInt fromInt
  where
  (w,h) = sz & width `view2` height
  toInt c = fromEnum $ w * c^.row + c^.col
  fromInt i = if c < w && r < h
    then Just $ mkCoord c r
    else Nothing
    where
    (r,c) = toEnum i `divMod` w

coordOnInt :: (DiscreteCoord c) => Size c -> (Int -> Int)
  -> Coord c -> Maybe (Coord c)
coordOnInt sz = underPrism $ coordInt sz

allCoords :: (DiscreteCoord c) => Size c -> [Coord c]
allCoords sz = enumCoords sz [0..lexMaxBound sz]

coordGrid :: (CoordType c) => Size c -> [[Coord c]]
coordGrid = coordArray Nothing Nothing

coordGridChunksAt :: (CoordType c)
  => Size c -> Coord c -> Size c -> [[Coord c]]
coordGridChunksAt chunkSize origin =
  coordArray (Just chunkSize) (Just origin)

coordGridChunks :: (CoordType c)
  => Size c -> Size c -> [[Coord c]]
coordGridChunks chunkSize = coordArray (Just chunkSize) Nothing

coordGridAt :: (CoordType c)
  => Coord c -> Size c -> [[Coord c]]
coordGridAt origin = coordArray Nothing (Just origin)

coordArray :: (CoordType c) => Maybe (Size c) -> Maybe (Coord c)
  -> Size c -> [[Coord c]]
coordArray chunkSize origin arrSize =
  [ [ mkCoord x y | x <- view width cs ] | y <- view height cs ]
  where
  ori = view coordSize $ fromMaybe 0 origin
  as = arrSize - 1
  cs = case chunkSize of
    Just sz -> enumFromThenTo <$> ori <*> (sz + ori) <*> (as + ori)
    Nothing -> enumFromTo     <$> ori                <*> (as + ori)

enumCoords :: (DiscreteCoord c) => Size c -> [Int] -> [Coord c]
enumCoords = mapMaybe . indexCoord

coordIndex :: (DiscreteCoord c) => Size c -> Coord c -> Int
coordIndex = review . coordInt

indexCoord :: (DiscreteCoord c) => Size c -> Int -> Maybe (Coord c)
indexCoord = preview . coordInt

lexMinBound :: Size c -> Int
lexMinBound _  = 0

lexMaxBound :: (DiscreteCoord c) => Size c -> Int
lexMaxBound sz = coordIndex sz $ Coord $ size (sz - 1)

coordSize :: Iso (Coord c) (Coord c) (Size c) (Size c)
coordSize = iso (Size . coord) (Coord . size)

-- }}}

-- V2 Maps / Folds {{{

projX :: (R2 f, Num a) => f a -> f a
projX = _y .~ 0

projY :: (R1 f, Num a) => f a -> f a
projY = _x .~ 0

reflX :: (R2 f, Num a) => f a -> f a
reflX = _y %~ negate

reflY :: (R1 f, Num a) => f a -> f a
reflY = _x %~ negate

reflId :: (R2 f, Num a) => f a -> f a
reflId = (_x %~ negate) . swapV2

foldV2 :: (R2 f) => (a -> a -> b) -> f a -> b
foldV2 f c = f (c^._x) (c^._y)

zipWithV2 :: (Additive f) => (a -> b -> c) -> f a -> f b -> f c
zipWithV2 = liftI2

swapV2 :: (R2 f) => f a -> f a
swapV2 v = (_x .~ (v ^. _y)) . (_y .~ (v ^._x)) $ v

-- }}}

-- Pretty Printing {{{

ppCoord :: Show c => Coord c -> String
ppCoord c = show (c^.col) ++ "," ++ show (c^.row)

ppSize :: Show c => Size c -> String
ppSize s = show (s^.width) ++ "x" ++ show (s^.height)

-- }}}

