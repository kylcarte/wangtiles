{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TileSet where

import Error
import Util

import Control.Applicative
import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as I
import qualified Data.Foldable as F
import qualified Data.Traversable as T

newtype TileSet a = TileSet
  { tileSet :: I.IntMap a
  } deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

-- Building {{{

tsFromMap :: I.IntMap a -> TileSet a
tsFromMap = TileSet

emptyTileSet :: TileSet a
emptyTileSet = tsFromMap I.empty

tsFromOrderedList :: [a] -> TileSet a
tsFromOrderedList = tsFromList . zip [0..]

tsFromListExcept :: [(Int,Int)] -> [[a]] -> TileSet a
tsFromListExcept es =
    tsFromList
  . zip [0..]
  . concat
  . deleteGridIndices es

tsFromList :: [(TileIndex,a)] -> TileSet a
tsFromList = tsFromMap . I.fromList

tsGenerate :: (TileIndex -> a) -> TileSet a
tsGenerate f = tsFromList $ zip is as
  where
  is = [0..]
  as = map f is

-- }}}

-- Accessing {{{

tsSize :: TileSet a -> TileIndex
tsSize = I.size . tileSet

tsAssocs :: TileSet a -> [(TileIndex,a)]
tsAssocs = I.assocs . tileSet

tsValues :: TileSet a -> [a]
tsValues = I.elems . tileSet

tsIndex :: TileSet a -> TileIndex -> a
tsIndex ts i = fromMaybe (error err) $ I.lookup i $ tileSet ts
  where
  err = "key " ++ show i ++ " is not in TileSet"

tsLookup' :: (Monad m) => TileSet a -> TileIndex -> ErrorT m a
tsLookup' ts i = reportNothing err $ I.lookup i $ tileSet ts
  where
  err = "Couldn't find index " ++ show i ++ " in TileSet"

tsLookup :: (Monad m, Show a) => TileSet a -> TileIndex -> ErrorT m a
tsLookup ts i = reportNothing err $ I.lookup i $ tileSet ts
  where
  err = "Couldn't find index " ++ show i ++ " in TileSet:\n" ++ ppTileSet ts

tsGetSingle :: TileSet a -> Maybe (TileIndex,a)
tsGetSingle ts
  | [a] <- tsAssocs ts
  = Just a
  | otherwise
  = Nothing

-- }}}

-- Maps / Traversals {{{

tsOnMap :: (I.IntMap a -> I.IntMap b) -> TileSet a -> TileSet b
tsOnMap f = tsFromMap . f . tileSet

tsOnMapA :: (Applicative f) => (I.IntMap a -> f (I.IntMap b)) -> TileSet a -> f (TileSet b)
tsOnMapA f = fmap tsFromMap . f . tileSet

tsOnMapM :: (Monad m) => (I.IntMap a -> m (I.IntMap b)) -> TileSet a -> m (TileSet b)
tsOnMapM f = (return . tsFromMap) <=< (f . tileSet)

tsMap :: (a -> b) -> TileSet a -> TileSet b
tsMap = tsOnMap . fmap

tsFilter :: (a -> Bool) -> TileSet a -> TileSet a
tsFilter = tsOnMap . I.filter

tsTraverseWithKey :: (Applicative f) => (TileIndex -> a -> f b) -> TileSet a -> f (TileSet b)
tsTraverseWithKey = tsOnMapA . I.traverseWithKey

{-
tsMapMWithKey :: (Monad f) => (TileIndex -> a -> f b) -> TileSet a -> f (TileSet b)
tsMapMWithKey = tsOnMapM . 
-}

-- }}}

-- Zips {{{

tsZipWithL :: (a -> b -> c) -> TileSet a -> TileSet b -> Maybe (TileSet c)
tsZipWithL f ta tb = fmap tsFromMap $ I.traverseWithKey g $ tileSet ta
  where
  g i a = f a <$> I.lookup i (tileSet tb)

tsZipWithR :: (a -> b -> c) -> TileSet a -> TileSet b -> Maybe (TileSet c)
tsZipWithR f ta tb = fmap tsFromMap
  $ I.traverseWithKey g $ tileSet tb
  where
  g i b = f <$> I.lookup i (tileSet ta) <*> pure b

tsZipL :: TileSet a -> TileSet b -> Maybe (TileSet (a,b))
tsZipL = tsZipWithL (,)

tsZipR :: TileSet a -> TileSet b -> Maybe (TileSet (a,b))
tsZipR = tsZipWithR (,)

-- }}}

-- Pretty Printing {{{

ppTileSet :: (Show a) => TileSet a -> String
ppTileSet = ppPadStringRows2Col . tsAssocs

-- }}}

