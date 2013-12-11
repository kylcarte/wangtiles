{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tile where

import Util

import Control.Applicative
import Control.Monad
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified System.Random as R

type Coord = (Int,Int)

col,row :: Coord -> Int
col = fst
row = snd

-- compare Coords lexicographically, row over column
compareCoordsLexi :: Coord -> Coord -> Ordering
compareCoordsLexi (c1,r1) (c2,r2) = case r of
  EQ -> compare c1 c2
  _  -> r
  where
  r = compare r1 r2

type Size  = (Int,Int)

type TileIndex = Int

-- TileSet {{{

newtype TileSet a = TileSet
  { tileSet :: I.IntMap a
  } deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

tsSize :: TileSet a -> Int
tsSize = I.size . tileSet

tsFromList :: [(Int,a)] -> TileSet a
tsFromList = TileSet . I.fromList

tsAssocs :: TileSet a -> [(Int,a)]
tsAssocs = I.assocs . tileSet

tsIndex :: TileSet a -> Int -> a
tsIndex ts i = tileSet ts I.! i

tsRandomIndex :: TileSet a -> Random Int
tsRandomIndex = randomKey . tileSet

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

{-
textureAt :: TileSet (Picture,a) -> TileIndex -> Picture
textureAt ts i = fst $ tileTexture ts i
-}

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

gridSubMap :: Eq a => a -> Grid a -> Grid a
gridSubMap a = Grid . M.filter (== a) . fromGrid

gridFromList :: [(Coord,a)] -> Grid a
gridFromList = Grid . M.fromList

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

gridCoords :: Grid a -> [Coord]
gridCoords = M.keys . fromGrid

gridFilter :: (a -> Bool) -> Grid a -> Grid a
gridFilter f = Grid . M.filter f . fromGrid

gridIndex :: Grid a -> Coord -> a
gridIndex g c = fromGrid g M.! c

gridLookup :: Grid a -> Coord -> Maybe a
gridLookup g c = M.lookup c (fromGrid g)

gridUpdate :: Grid a -> [(Coord,a)] -> Grid a
gridUpdate g us = Grid $ fromGrid g `M.union` M.fromList us

gridContents :: Grid a -> [(Coord,a)]
gridContents = M.assocs . fromGrid

gridUpdateByM :: (Monad m) => [Coord]
  -> (a -> m a) -> Grid a -> m (Grid a)
gridUpdateByM cs f g = do
  let m = fromGrid g
  m' <- mapUpdateByM cs f m
  return $ Grid m'

gridUpdateWithKeyByM :: (Monad m) => [Coord]
  -> (Coord -> a -> m a) -> Grid a -> m (Grid a)
gridUpdateWithKeyByM cs f =
  (return . Grid <=< mapUpdateWithKeyByM cs f) . fromGrid

randomGrid :: forall a. R.Random a => (a,a) -> Size -> Random (Grid a)
randomGrid rng sz = T.traverse f . mkEmptyGrid sz =<< random
  where
  f :: a -> Random a
  f = const $ randomR rng

ppGrid :: Show a => Grid a -> String
ppGrid g = ppRows
  [ [ pad s
    | x <- [0..c-1]
    , let s = show $ gridIndex g (x,y)
    ]
  | y <- [0..r-1]
  ]
  where
  (c,r) = gridSize g
  rdigits = fromEnum (logBase 10 $ toEnum r :: Float)
  pad s = replicate (rdigits - length s) ' ' ++ s

-- }}}

-- TileMap {{{

newtype TileMap = TileMap
  { tileMap :: Grid TileIndex
  } deriving (Eq,Show)

mkEmptyTileMap :: Size -> TileMap
mkEmptyTileMap sz = TileMap { tileMap = mkEmptyGrid sz 0 }

tmSubMap :: TileIndex -> TileMap -> TileMap
tmSubMap ti = TileMap . gridSubMap ti . tileMap

tmFromList :: [(Coord,TileIndex)] -> TileMap
tmFromList = TileMap . gridFromList

tmFilter :: (TileIndex -> Bool) -> TileMap -> TileMap
tmFilter f = TileMap . gridFilter f . tileMap

tmSize :: TileMap -> Size
tmSize = gridSize . tileMap

tmRows :: TileMap -> Int
tmRows = rows . tileMap

tmCols :: TileMap -> Int
tmCols = cols . tileMap

tmCoords :: TileMap -> [Coord]
tmCoords = gridCoords . tileMap

tmAssocs :: TileMap -> [(Coord,TileIndex)]
tmAssocs = gridContents . tileMap

tmIndex :: TileMap -> Coord -> TileIndex
tmIndex tm c = gridIndex (tileMap tm) c

tmUpdate :: [(Coord,TileIndex)] -> TileMap -> TileMap
tmUpdate ts tm = tm { tileMap = gridUpdate (tileMap tm) ts }

tmUpdate1 :: Coord -> TileIndex -> TileMap -> TileMap
tmUpdate1 xy t = tmUpdate [(xy,t)]

tmUpdateByM :: (Monad m) => [Coord]
  -> (TileIndex -> m TileIndex) -> TileMap -> m TileMap
tmUpdateByM cs f = (return . TileMap <=< gridUpdateByM cs f) . tileMap

tmUpdateWithKeyByM :: (Monad m) => [Coord]
  -> (Coord -> TileIndex -> m TileIndex) -> TileMap -> m TileMap
tmUpdateWithKeyByM cs f =
  (return . TileMap <=< gridUpdateWithKeyByM cs f) . tileMap

tmTraverse :: (Applicative f) => (TileIndex -> f TileIndex)
  -> TileMap -> f TileMap
tmTraverse f = fmap TileMap . T.traverse f . tileMap

randomTileMap :: (TileIndex,TileIndex) -> Size -> Random TileMap
randomTileMap rng = fmap TileMap . randomGrid rng

ppTileMap :: TileMap -> String
ppTileMap = ppGrid . tileMap

-- }}}

