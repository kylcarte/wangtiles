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
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified System.Random as R

newtype Grid c a = Grid
  { _grid :: M.Map (Coord c) a
  } deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

makeLenses ''Grid

type Coords c = Grid c ()

csFromList :: (Ord c) => [Coord c] -> Coords c
csFromList cs = gridFromList $ zip cs $ repeat ()

-- Grid {{{

mkEmptyGrid :: (Integral c) => Size c -> a -> Grid c a
mkEmptyGrid sz a = gridFromList $ zip cs $ repeat a
  where
  cs = enumCoords sz [0..lexMaxBound sz]

mkIotaGrid :: (Enum a, Num a, Integral c) => Size c -> Grid c a
mkIotaGrid sz = gridFromList $ zip cs [0..]
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
    (+) 1
  . uncurry (-)
  . over both Size
  . (safeMaximum &&& safeMinimum)
  . map (view coordV2)
  . M.keys
  . _grid

gridRows, gridCols :: (Integral c) => Grid c a -> c
gridRows = view height . gridSize
gridCols = view  width . gridSize

gridOnMap :: (M.Map (Coord c) a -> M.Map (Coord c) b) -> Grid c a -> Grid c b
gridOnMap = (grid %~)

gridOnMapA :: (Applicative f) => (M.Map (Coord c) a -> f (M.Map (Coord c) b))
  -> Grid c a -> f (Grid c b)
gridOnMapA f = fmap Grid . f . _grid

gridOnMapM :: (Monad m) => (M.Map (Coord c) a -> m (M.Map (Coord c) b))
  -> Grid c a -> m (Grid c b)
gridOnMapM f = return . Grid <=< (f . _grid)

gridLookup :: (Ord c) => Grid c a -> Coord c -> Maybe a
gridLookup g c = M.lookup c $ _grid g

gridIndex :: (Ord c) => Grid c a -> Coord c -> a
gridIndex g c = _grid g M.! c

gridFilter :: (a -> Bool) -> Grid c a -> Grid c a
gridFilter pr = gridOnMap $ M.filter pr

gridFromList :: (Ord c) => [(Coord c,a)] -> Grid c a
gridFromList = Grid . M.fromList

gridContents :: Grid c a -> [(Coord c,a)]
gridContents = M.assocs . _grid

gridDifference :: (Ord c) => Grid c a -> Grid c b -> Grid c a
gridDifference = gridOnMap . M.difference . _grid

gridMinimum, gridMaximum :: (Ord c) => Grid c a -> Maybe (Coord c, a)
gridMinimum = fmap fst . M.minViewWithKey . _grid
gridMaximum = fmap fst . M.maxViewWithKey . _grid

-- }}}

-- Key Maps / Traversals {{{

gridKeys :: Grid c a -> [Coord c]
gridKeys = M.keys . _grid

gridTraverseWithKey :: (Applicative f) => (Coord c -> a -> f b)
  -> Grid c a -> f (Grid c b)
gridTraverseWithKey f = gridOnMapA $ M.traverseWithKey f

gridMapKeysTo :: (Coord c -> a) -> Grid c b -> Grid c a
gridMapKeysTo = gridOnMap . mapKeysTo

gridTraverseKeys :: (Applicative f, Ord c) =>
  (Coord c -> f a) -> Grid c b -> f (Grid c a)
gridTraverseKeys = gridOnMapA . traverseKeys



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
gridSubMap cs = gridOnMap $ subMap (_grid cs)

gridSubMapByValue :: (Eq a, Ord c) => a -> Grid c a -> Grid c a
gridSubMapByValue a = gridOnMap $ subMapByValue a

subMap :: (Ord k) => M.Map k b -> M.Map k a -> M.Map k a
subMap = filterKeys . flip M.member

subMapByValue :: (Eq a, Ord k) => a -> M.Map k a -> M.Map k a
subMapByValue = M.filter . (==)

-- }}}

-- Pretty Printing {{{

ppGrid :: (Integral c, Show a, Enum a) => Grid c a -> String
ppGrid g = ppRows
  [ [ pad s
    | c <- [0..(sz^.width) - 1]
    , let s = fromMaybe " " $ fmap show $ gridLookup g $ mkCoord c r
    ]
  | r <- [0..(sz^.height) - 1]
  ]
  where
  sz = gridSize g
  mxdigits = length mx
  pad s = replicate (mxdigits - length s) ' ' ++ s
  mx = maybe "" (show . snd) $ gridMaximum g

-- }}}

