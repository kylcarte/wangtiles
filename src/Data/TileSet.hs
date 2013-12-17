{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TileSet where

import Control.Monad.Trans.Random
import Util

import Control.Applicative
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

-- }}}

-- Indexing {{{

tsSize :: TileSet a -> TileIndex
tsSize = I.size . tileSet

tsFromList :: [(TileIndex,a)] -> TileSet a
tsFromList = tsFromMap . I.fromList

tsAssocs :: TileSet a -> [(TileIndex,a)]
tsAssocs = I.assocs . tileSet

tsIndex :: TileSet a -> TileIndex -> a
tsIndex ts i = case I.lookup i $ tileSet ts of
  Just a -> a
  Nothing -> error $ "key " ++ show i ++ " is not in TileSet"

tsLookup :: TileSet a -> TileIndex -> Maybe a
tsLookup ts i = I.lookup i $ tileSet ts

tsGetSingle :: TileSet a -> Maybe (TileIndex,a)
tsGetSingle ts
  | [a] <- tsAssocs ts
  = Just a
  | otherwise
  = Nothing

-- }}}

-- Random {{{

tsRandomIndex :: TileSet a -> Random TileIndex
tsRandomIndex = randomKey . tileSet

-- }}}

-- Mapping {{{

tsMap :: (a -> b) -> TileSet a -> TileSet b
tsMap f = tsFromMap . fmap f . tileSet

tsFilter :: (a -> Bool) -> TileSet a -> TileSet a
tsFilter f = tsFromMap . I.filter f . tileSet

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

