{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Tile where

import Util

import Control.Applicative
import Control.Arrow (first,second)
import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified System.Random as R

import Text.Show.Pretty

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

tsSize :: TileSet a -> TileIndex
tsSize = I.size . tileSet

tsFromList :: [(TileIndex,a)] -> TileSet a
tsFromList = TileSet . I.fromList

tsAssocs :: TileSet a -> [(TileIndex,a)]
tsAssocs = I.assocs . tileSet

tsIndex :: Show a => TileSet a -> TileIndex -> a
tsIndex ts i = case I.lookup i $ tileSet ts of
  Just a -> a
  Nothing -> error $ "key " ++ show i ++ " is not in TileSet: " ++ ppShow ts

tsGetSingle :: TileSet a -> Maybe (TileIndex,a)
tsGetSingle ts
  | [a] <- tsAssocs ts
  = Just a
  | otherwise
  = Nothing

tsRandomIndex :: TileSet a -> Random TileIndex
tsRandomIndex = randomKey . tileSet

tsMap :: (a -> b) -> TileSet a -> TileSet b
tsMap f = TileSet . fmap f . tileSet

tsFilter :: (a -> Bool) -> TileSet a -> TileSet a
tsFilter f = TileSet . I.filter f . tileSet

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
    onPair (succ . maximum)
  . unzip
  . M.keys
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

gridContents :: Grid a -> [(Coord,a)]
gridContents = M.assocs . fromGrid

gridUpdateWithKey :: (Coord -> a -> a) -> Grid a -> Grid a
gridUpdateWithKey f = Grid . M.mapWithKey f . fromGrid

gridUpdateAt :: [Coord] -> (a -> a) -> Grid a -> Grid a
gridUpdateAt cs f = Grid . mapUpdateAt cs f . fromGrid

gridUpdateWithKeyAt :: [Coord] -> (Coord -> a -> a)
  -> Grid a -> Grid a
gridUpdateWithKeyAt cs f = Grid . mapUpdateWithKeyAt cs f . fromGrid

gridUpdateAtM :: (Monad m) => [Coord]
  -> (a -> m a) -> Grid a -> m (Grid a)
gridUpdateAtM cs f g = do
  let m = fromGrid g
  m' <- mapUpdateAtM cs f m
  return $ Grid m'

gridUpdateWithKeyAtM :: (Monad m) => [Coord]
  -> (Coord -> a -> m a) -> Grid a -> m (Grid a)
gridUpdateWithKeyAtM cs f =
  (return . Grid <=< mapUpdateWithKeyAtM cs f) . fromGrid

gridTraverseWithKey :: (Applicative m) => (Coord -> a -> m a)
  -> Grid a -> m (Grid a)
gridTraverseWithKey f = fmap Grid . M.traverseWithKey f . fromGrid

randomGrid :: forall a. R.Random a => (a,a) -> Size -> Random (Grid a)
randomGrid rng sz = T.traverse f . mkEmptyGrid sz =<< random
  where
  f :: a -> Random a
  f = const $ randomR rng

ppGrid :: Show a => Grid a -> String
ppGrid g = ppRows
  [ [ pad s
    | x <- [0..c-1]
    , let s = fromMaybe " " $ fmap show $ gridLookup g (x,y)
    ]
  | y <- [0..r-1]
  ]
  where
  (c,r) = gridSize g
  rdigits = fromEnum (logBase 10 $ toEnum r :: Float)
  pad s = replicate (rdigits - length s) ' ' ++ s

-- }}}

-- Surrounding {{{

data Surrounding a = Surrounding
  { sC  :: a
  , sNW :: Maybe a
  , sN  :: Maybe a
  , sNE :: Maybe a
  , sE  :: Maybe a
  , sSE :: Maybe a
  , sS  :: Maybe a
  , sSW :: Maybe a
  , sW  :: Maybe a
  } deriving (Eq,Show)

gridSurrounding :: Grid a -> Coord -> Surrounding a
gridSurrounding g c = Surrounding
  { sC  = gridIndex g c
  , sNW = lu . n.w $ c
  , sN  = lu . n   $ c
  , sNE = lu . n.e $ c
  , sE  = lu . e   $ c
  , sSE = lu . s.e $ c
  , sS  = lu . s   $ c
  , sSW = lu . s.w $ c
  , sW  = lu . w   $ c
  }
  where
  lu = gridLookup g
  n = second pred
  s = second succ
  w = first  pred
  e = first  succ

tmSurrounding :: TileMap -> Coord -> Surrounding TileIndex
tmSurrounding = gridSurrounding . tileMap

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

tmUpdate :: (TileIndex -> TileIndex) -> TileMap -> TileMap
tmUpdate f = TileMap . fmap f . tileMap

tmUpdateWithKey :: (Coord -> TileIndex -> TileIndex)
  -> TileMap -> TileMap
tmUpdateWithKey f = TileMap . gridUpdateWithKey f . tileMap

tmUpdateAt :: [Coord] -> (TileIndex -> TileIndex) -> TileMap -> TileMap
tmUpdateAt cs f = TileMap . gridUpdateAt cs f . tileMap

tmUpdateWithKeyAt :: [Coord] -> (Coord -> TileIndex -> TileIndex)
  -> TileMap -> TileMap
tmUpdateWithKeyAt cs f = TileMap . gridUpdateWithKeyAt cs f . tileMap

tmUpdateAtM :: (Monad m) => [Coord]
  -> (TileIndex -> m TileIndex) -> TileMap -> m TileMap
tmUpdateAtM cs f = (return . TileMap <=< gridUpdateAtM cs f) . tileMap

tmTraverseWithKey :: (Applicative m) => (Coord -> TileIndex -> m TileIndex)
  -> TileMap -> m TileMap
tmTraverseWithKey f = fmap TileMap . gridTraverseWithKey f . tileMap

tmUpdateWithKeyAtM :: (Monad m) => [Coord]
  -> (Coord -> TileIndex -> m TileIndex) -> TileMap -> m TileMap
tmUpdateWithKeyAtM cs f =
  (return . TileMap <=< gridUpdateWithKeyAtM cs f) . tileMap

tmTraverse :: (Applicative f) => (TileIndex -> f TileIndex)
  -> TileMap -> f TileMap
tmTraverse f = fmap TileMap . T.traverse f . tileMap

randomTileMap :: (TileIndex,TileIndex) -> Size -> Random TileMap
randomTileMap rng = fmap TileMap . randomGrid rng

ppTileMap :: TileMap -> String
ppTileMap = ppGrid . tileMap

-- }}}

