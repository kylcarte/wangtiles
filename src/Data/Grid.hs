{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Data.Grid where

import Data.Points
import Util

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Data.List (maximumBy, minimumBy, nub)
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T

newtype Grid c a = Grid
  { grid :: M.Map (Coord c) a
  } deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

type Coords c = Grid c ()

csFromList :: (Ord c) => [Coord c] -> Coords c
csFromList cs = gridFromList $ zip cs $ repeat ()

-- Grid {{{

gridFromMap :: M.Map (Coord c) a -> Grid c a
gridFromMap = Grid

emptyGrid :: Grid c a
emptyGrid = gridFromMap M.empty

mkRepeatGrid :: (DiscreteCoord c) => Size c -> a -> Grid c a
mkRepeatGrid sz a = gridFromList $ zip cs $ repeat a
  where
  cs = allCoords sz

mkIotaGrid :: (DiscreteCoord c) => Size c -> Grid c TileIndex
mkIotaGrid sz = gridFromList $ zip cs [0..]
  where
  cs = allCoords sz

gridInsert :: (Ord c) => Coord c -> a -> Grid c a -> Grid c a
gridInsert c = gridOnMap . M.insert c

gridUnion :: (Ord c) => Grid c a -> Grid c a -> Grid c a
gridUnion g1 = gridFromMap . M.union (grid g1) . grid

gridUnions :: (Ord c) => [Grid c a] -> Grid c a
gridUnions = gridFromMap . M.unions . map grid

gridSize :: (CoordType c) => Grid c a -> Size c
gridSize =
    (+) 1
  . uncurry (-)
  . over both (view coordSize)
  . (safeMaximumV2 0 &&& safeMinimumV2 0)
  . gridCoords

gridRows, gridCols :: (CoordType c) => Grid c a -> c
gridRows = view height . gridSize
gridCols = view  width . gridSize

gridOnMap :: (M.Map (Coord c) a -> M.Map (Coord c) b) -> Grid c a -> Grid c b
gridOnMap f g = g { grid = f $ grid g }

gridOnMapA :: (Applicative f) => (M.Map (Coord c) a -> f (M.Map (Coord c) b))
  -> Grid c a -> f (Grid c b)
gridOnMapA f = fmap gridFromMap . f . grid

gridOnMapM :: (Monad m) => (M.Map (Coord c) a -> m (M.Map (Coord c) b))
  -> Grid c a -> m (Grid c b)
gridOnMapM f = return . gridFromMap <=< (f . grid)

gridLookup :: (CoordType c) => Grid c a -> Coord c -> Maybe a
gridLookup g c = M.lookup c $ grid g

gridIndex :: (Ord c) => Grid c a -> Coord c -> a
gridIndex g c = grid g M.! c

gridFilter :: (a -> Bool) -> Grid c a -> Grid c a
gridFilter pr = gridOnMap $ M.filter pr

gridFromList :: (Ord c) => [(Coord c,a)] -> Grid c a
gridFromList = gridFromMap . M.fromList

gridCoords :: Grid c a -> [Coord c]
gridCoords = M.keys . grid

gridContents :: Grid c a -> [(Coord c,a)]
gridContents = M.assocs . grid

gridList :: (CoordType c) => Grid c a -> [[Maybe a]]
gridList g = map (map $ gridLookup g)
  $ coordGrid
  $ gridSize g

gridDifference :: (Ord c) => Grid c a -> Grid c b -> Grid c a
gridDifference = gridOnMap . M.difference . grid

gridMinimum, gridMaximum :: (Ord c) => Grid c a -> Maybe (Coord c, a)
gridMinimum = fmap fst . M.minViewWithKey . grid
gridMaximum = fmap fst . M.maxViewWithKey . grid

gridMinimumValue, gridMaximumValue :: (Ord a, Ord c)
  => Grid c a -> Maybe (Coord c, a)
gridMinimumValue g
  | cs <- gridContents g
  = if null cs
    then Nothing
    else Just $ minimumBy (compare `on` snd) cs
gridMaximumValue g
  | cs <- gridContents g
  = if null cs
    then Nothing
    else Just $ maximumBy (compare `on` snd) cs

-- }}}

-- Key Maps / Traversals {{{

gridKeys :: Grid c a -> [Coord c]
gridKeys = M.keys . grid

gridValues :: Eq a => Grid c a -> [a]
gridValues = nub . M.elems . grid

gridTraverseWithKey :: (Applicative f) => (Coord c -> a -> f b)
  -> Grid c a -> f (Grid c b)
gridTraverseWithKey f = gridOnMapA $ M.traverseWithKey f

gridMapKeysTo :: (Coord c -> a) -> Grid c b -> Grid c a
gridMapKeysTo = gridOnMap . mapKeysTo

gridTraverseKeys :: (Applicative f, Ord c)
  => (Coord c -> f a) -> Grid c b -> f (Grid c a)
gridTraverseKeys = gridOnMapA . traverseKeys



filterKeys :: (k -> Bool) -> M.Map k a -> M.Map k a
filterKeys = M.filterWithKey . onlyIndex

mapKeysTo :: (k -> a) -> M.Map k b -> M.Map k a
mapKeysTo = M.mapWithKey . onlyIndex

foldrKeys :: (k -> b -> b) -> b -> M.Map k a -> b
foldrKeys = M.foldrWithKey . onlyIndex

traverseKeys :: (Applicative f, Ord k)
  => (k -> f a) -> M.Map k b -> f (M.Map k a)
traverseKeys = M.traverseWithKey . onlyIndex



gridFoldrWithKey :: (Coord c -> b -> a -> a)
  -> a -> Grid c b -> a
gridFoldrWithKey f a = M.foldrWithKey f a . grid

gridFoldrKeys :: (Coord c -> a -> a) -> a -> Grid c b -> a
gridFoldrKeys f a = M.foldrWithKey (const . f) a . grid

-- }}}

-- Selective Update {{{

gridUpdateAt :: (Ord c) => [Coord c] -> (a -> a) -> Grid c a -> Grid c a
gridUpdateAt cs = gridOnMap . updateAt cs

gridUpdateWithKeyAt :: (Ord c) => [Coord c] -> (Coord c -> a -> a)
  -> Grid c a -> Grid c a
gridUpdateWithKeyAt cs = gridOnMap . updateWithKeyAt cs

gridUpdateAtM :: (Monad m, Ord c) => [Coord c] -> (a -> m a)
  -> Grid c a -> m (Grid c a)
gridUpdateAtM cs = gridOnMapM . updateAtM cs

gridUpdateWithKeyAtM :: (Monad m, Ord c) => [Coord c]
  -> (Coord c -> a -> m a) -> Grid c a -> m (Grid c a)
gridUpdateWithKeyAtM cs = gridOnMapM . updateWithKeyAtM cs



updateAt :: (Ord k) => [k] -> (a -> a) -> M.Map k a -> M.Map k a
updateAt ks = updateWithKeyAt ks . const

updateWithKeyAt :: (Ord k) => [k] -> (k -> a -> a)
  -> M.Map k a -> M.Map k a
updateWithKeyAt ks f mp = F.foldl fn mp ks
  where
  fn m k = M.insert k (f k $ m M.! k) m

updateAtM :: (Monad m, Ord k) => [k] -> (a -> m a)
  -> M.Map k a -> m (M.Map k a)
updateAtM ks = updateWithKeyAtM ks . const

updateWithKeyAtM :: (Monad m, Ord k) => [k] -> (k -> a -> m a)
  -> M.Map k a -> m (M.Map k a)
updateWithKeyAtM ks f mp = F.foldlM fn mp ks
  where
  fn m k = do
    a <- maybe err return $ M.lookup k m
    b <- f k a
    return $ M.insert k b m
  err = fail "key not in map"

-- }}}

-- SubMap {{{

gridSubMap :: (Ord c) => Grid c b -> Grid c a -> Grid c a
gridSubMap cs = gridOnMap $ mSubMap (grid cs)

gridSubMapByValue :: (Eq a, Ord c) => a -> Grid c a -> Grid c a
gridSubMapByValue a = gridOnMap $ subMapByValue a

mSubMap :: (Ord k) => M.Map k b -> M.Map k a -> M.Map k a
mSubMap = filterKeys . flip M.member

subMapByValue :: (Eq a, Ord k) => a -> M.Map k a -> M.Map k a
subMapByValue = M.filter . (==)

-- }}}

-- Pretty Printing {{{

ppGrid :: (CoordType c, Ord a, Show a, Enum a) => Grid c a -> String
ppGrid = ppSparseRows . gridList

-- }}}

