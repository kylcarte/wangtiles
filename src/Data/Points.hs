{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Points where

import Util

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Random
import Data.Aeson
import Data.Foldable (Foldable(..))
import Data.Maybe (mapMaybe)
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

diagonal :: c -> Coord c
diagonal c = mkCoord c c

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
    ( Eq, Ord, Show, Num, Fractional
    , Epsilon, R1, R2, Additive
    , Functor, Applicative, Monad
    , Foldable, Traversable
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

coordInt :: (Integral c) => Size c -> Prism' Int (Coord c)
coordInt sz = prism' toInt fromInt
  where
  (w,h) = sz & width `view2` height
  toInt c = fromEnum $ w * c^.row + c^.col
  fromInt i = if c < w && r < h
    then Just $ mkCoord c r
    else Nothing
    where
    (r,c) = toEnum i `divMod` w

coordOnInt :: (Integral c) => Size c -> (Int -> Int)
  -> Coord c -> Maybe (Coord c)
coordOnInt sz = underPrism $ coordInt sz

allCoords :: (Integral c) => Size c -> [Coord c]
allCoords sz = enumCoords sz [0..lexMaxBound sz]

coordGrid :: (Integral c) => Size c -> [[Coord c]]
coordGrid = coordMat Nothing

coordGridChunks :: (Integral c) => Size c -> Size c -> [[Coord c]]
coordGridChunks = coordMat . Just

coordMat :: (Integral c) => Maybe (Size c) -> Size c -> [[Coord c]]
coordMat mt sz =
  [ [ mkCoord x y
    | x <- xs $ view width <$> mt
    ]
  | y <- ys $ view height <$> mt
  ]
  where
  (w,h) = view widthHeight $ sz - 1
  xs = maybe [0..w] $ \tw -> [0,tw..w]
  ys = maybe [0..h] $ \th -> [0,th..h]

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

