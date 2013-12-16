{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Grid where

import Control.Monad.Trans.Random
import Data.Points
import Util

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified System.Random as R

newtype Grid c a = Grid
  { _grid :: M.Map (Coord c) a
  } deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

makeLenses ''Grid

type Coords c = Grid c ()

-- Grid {{{

mkEmptyGrid :: (Integral c) => Size c -> a -> Grid c a
mkEmptyGrid sz a = Grid $ M.fromList $ zip cs $ repeat a
  where
  cs = enumCoords sz [0..lexMaxBound sz]

mkRandomGrid :: forall a c. (Integral c, R.Random a) =>
  (a,a) -> Size c -> Random (Grid c a)
mkRandomGrid rng sz = T.traverse f . mkEmptyGrid sz =<< random
  where
  f :: a -> Random a
  f = const $ randomR rng

gridSize :: (Integral c) => Grid c a -> Size c
gridSize =
    uncurry (-)
  . over both Size
  . (safeMinimum &&& safeMaximum)
  . map (view coordV2)
  . M.keys
  . _grid

gridRows, gridCols :: (Integral c) => Grid c a -> c
gridRows = view height . gridSize
gridCols = view  width . gridSize

gridMap :: (M.Map (Coord c) a -> M.Map (Coord c) b) -> Grid c a -> Grid c b
gridMap = (grid %~)

gridMapA :: (Applicative f) => (M.Map (Coord c) a -> f (M.Map (Coord c) b))
  -> Grid c a -> f (Grid c b)
gridMapA f = fmap Grid . f . _grid

gridMapM :: (Monad m) => (M.Map (Coord c) a -> m (M.Map (Coord c) b))
  -> Grid c a -> m (Grid c b)
gridMapM f = return . Grid <=< (f . _grid)

gridLookup :: (Ord c) => Grid c a -> Coord c -> Maybe a
gridLookup g c = M.lookup c $ _grid g

gridIndex :: (Ord c) => Grid c a -> Coord c -> a
gridIndex g c = _grid g M.! c

gridFilter :: (a -> Bool) -> Grid c a -> Grid c a
gridFilter pr = gridMap $ M.filter pr

gridFromList :: (Ord c) => [(Coord c,a)] -> Grid c a
gridFromList = Grid . M.fromList

gridContents :: Grid c a -> [(Coord c,a)]
gridContents = M.assocs . _grid

-- }}}

-- Key Maps / Traversals {{{

gridKeys :: Grid c a -> [Coord c]
gridKeys = M.keys . _grid

gridTraverseWithKey :: (Applicative f) => (Coord c -> a -> f b)
  -> Grid c a -> f (Grid c b)
gridTraverseWithKey f = gridMapA $ M.traverseWithKey f

gridMapKeysTo :: (Coord c -> a) -> Grid c b -> Grid c a
gridMapKeysTo = gridMap . mapKeysTo

gridTraverseKeys :: (Applicative f, Ord c) =>
  (Coord c -> f a) -> Grid c b -> f (Grid c a)
gridTraverseKeys = gridMapA . traverseKeys



filterKeys :: (k -> Bool) -> M.Map k a -> M.Map k a
filterKeys = M.filterWithKey . onlyIndex

mapKeysTo :: (k -> a) -> M.Map k b -> M.Map k a
mapKeysTo = M.mapWithKey . onlyIndex

foldrKeys :: (k -> b -> b) -> b -> M.Map k a -> b
foldrKeys = M.foldrWithKey . onlyIndex

traverseKeys :: (Applicative f, Ord k) =>
  (k -> f a) -> M.Map k b -> f (M.Map k a)
traverseKeys = M.traverseWithKey . onlyIndex

-- }}}

-- Selective Update {{{

gridUpdateAt :: (Ord c) => [Coord c] -> (a -> a) -> Grid c a -> Grid c a
gridUpdateAt cs = gridMap . updateAt cs

gridUpdateWithKeyAt :: (Ord c) => [Coord c] -> (Coord c -> a -> a)
  -> Grid c a -> Grid c a
gridUpdateWithKeyAt cs = gridMap . updateWithKeyAt cs

gridUpdateAtM :: (Monad m, Ord c) => [Coord c] -> (a -> m a)
  -> Grid c a -> m (Grid c a)
gridUpdateAtM cs = gridMapM . updateAtM cs

gridUpdateWithKeyAtM :: (Monad m, Ord c) => [Coord c]
  -> (Coord c -> a -> m a) -> Grid c a -> m (Grid c a)
gridUpdateWithKeyAtM cs = gridMapM . updateWithKeyAtM cs



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
gridSubMap cs = gridMap $ subMap (_grid cs)

gridSubMapByValue :: (Eq a, Ord c) => a -> Grid c a -> Grid c a
gridSubMapByValue a = gridMap $ subMapByValue a

subMap :: (Ord k) => M.Map k b -> M.Map k a -> M.Map k a
subMap = filterKeys . flip M.member

subMapByValue :: (Eq a, Ord k) => a -> M.Map k a -> M.Map k a
subMapByValue = M.filter . (==)

-- }}}

{-
ppGrid :: Show a => Grid c a -> String
ppGrid g = ppRows
  [ [ pad s
    | c <- [0..width sz - 1]
    , let s = fromMaybe " " $ fmap show $ gridLookup g $ mkCoord c r
    ]
  | r <- [0..height sz - 1]
  ]
  where
  sz = gridSize g
  rdigits = logBase 10 `withFloat` width sz
  pad s = replicate (rdigits - length s) ' ' ++ s
-}

