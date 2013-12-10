{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Tile where

import Util

import Control.Applicative
import Data.List (intercalate)
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T

type Coord = (Int,Int)
type Size  = (Int,Int)

type TileIndex = Int

-- TileSet {{{

newtype TileSet a = TileSet
  { tileSet :: I.IntMap a
  } deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

tsFromList :: [(Int,a)] -> TileSet a
tsFromList = TileSet . I.fromList

tsAssocs :: TileSet a -> [(Int,a)]
tsAssocs = I.assocs . tileSet

tsIndex :: TileSet a -> Int -> a
tsIndex ts i = tileSet ts I.! i

tsRandomKey :: TileSet a -> Random Int
tsRandomKey = randomKey . tileSet

tsZipWithL :: (a -> b -> c) -> TileSet a -> TileSet b -> Maybe (TileSet c)
tsZipWithL f (TileSet ta) (TileSet tb) = fmap TileSet $ I.traverseWithKey g ta
  where
  g i a = f a <$> I.lookup i tb

tsZipWithR :: (a -> b -> c) -> TileSet a -> TileSet b -> Maybe (TileSet c)
tsZipWithR f (TileSet ta) (TileSet tb) = fmap TileSet $ I.traverseWithKey g tb
  where
  g i b = f <$> I.lookup i ta <*> pure b

tsZipL :: TileSet a -> TileSet b -> Maybe (TileSet (a,b))
tsZipL = tsZipWithL (,)

tsZipR :: TileSet a -> TileSet b -> Maybe (TileSet (a,b))
tsZipR = tsZipWithR (,)

-- }}}

-- Grid {{{

newtype Grid a = Grid
  { fromGrid :: M.Map Coord a
  } deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

mkEmptyGrid :: Size -> a -> Grid a
mkEmptyGrid (c,r) a = Grid $ M.fromList
  [ ((x,y),a)
  | x <- [0..c-1]
  , y <- [0..r-1]
  ]

gridSize :: Grid a -> Size
gridSize =
    onPair succ
  . fst
  . M.findMax
  . fromGrid

rows :: Grid a -> Int
rows = snd . gridSize

cols :: Grid a -> Int
cols = fst . gridSize

gridIndex :: Grid a -> Coord -> a
gridIndex g c = fromGrid g M.! c

gridLookup :: Grid a -> Coord -> Maybe a
gridLookup g c = M.lookup c (fromGrid g)

gridUpdate :: Grid a -> [(Coord,a)] -> Grid a
gridUpdate g us = Grid $ fromGrid g `M.union` M.fromList us

gridContents :: Grid a -> [(Coord,a)]
gridContents = M.assocs . fromGrid

{-
gridNeighborhood :: Grid a -> Coord -> Neighborhood a
gridNeighborhood g c = N
  { nC  = gridIndex g c
  , nNW = gridLookup g 
-}

-- }}}

data Neighborhood a = N
  { nC  :: a
  , nNW :: Maybe a
  , nN  :: Maybe a
  , nNE :: Maybe a
  , nE  :: Maybe a
  , nSE :: Maybe a
  , nS  :: Maybe a
  , nSW :: Maybe a
  , nW  :: Maybe a
  } deriving (Eq,Show)

-- NeighborhoodPattern {{{

data NeighborhoodPattern = NP
  { npNW :: Pattern
  , npN  :: Pattern
  , npNE :: Pattern
  , npE  :: Pattern
  , npSE :: Pattern
  , npS  :: Pattern
  , npSW :: Pattern
  , npW  :: Pattern
  } deriving (Eq,Show)

data Pattern
  = S
  | D
  | A
  deriving (Eq,Show)

-- }}}

-- TileMap {{{

newtype TileMap = TileMap
  { tileMap :: Grid TileIndex
  } deriving (Eq,Show)

tmSize :: TileMap -> Size
tmSize = gridSize . tileMap

tmRows :: TileMap -> Int
tmRows = rows . tileMap

tmCols :: TileMap -> Int
tmCols = cols . tileMap

tmContents :: TileMap -> [(Coord,TileIndex)]
tmContents = gridContents . tileMap

tmIndex :: TileMap -> Coord -> TileIndex
tmIndex tm c = gridIndex (tileMap tm) c

tmUpdate :: [(Coord,TileIndex)] -> TileMap -> TileMap
tmUpdate ts tm = tm { tileMap = gridUpdate (tileMap tm) ts }

tmUpdate1 :: Coord -> TileIndex -> TileMap -> TileMap
tmUpdate1 xy t = tmUpdate [(xy,t)]

tmTraverse :: (Applicative f) => (TileIndex -> f TileIndex)
  -> TileMap -> f TileMap
tmTraverse f = fmap TileMap . T.traverse f . tileMap

mkEmptyTileMap :: Size -> TileMap
mkEmptyTileMap sz = TileMap { tileMap = mkEmptyGrid sz 0 }

randomTileMap :: (TileIndex,TileIndex) -> Size -> Random TileMap
randomTileMap rng = tmTraverse (const $ randomR rng) . mkEmptyTileMap 

ppTileMap :: TileMap -> String
ppTileMap tm = intercalate "\n"
  [ unwords
    [ pad s
    | x <- [0..c-1]
    , let s = show $ tmIndex tm (x,y)
    ]
  | y <- [0..r-1]
  ]
  where
  (c,r) = tmSize tm
  rdigits = fromEnum (logBase 10 $ toEnum r :: Float)
  pad s = replicate (rdigits - length s) ' ' ++ s

-- }}}

