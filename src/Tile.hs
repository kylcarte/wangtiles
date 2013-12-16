{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tile where

import Control.Monad.Trans.Random
import Data.Grid
import Data.Points
import Util

import Control.Applicative
import Control.Lens
import qualified Data.IntMap as I
import qualified Data.Foldable as F
import qualified Data.Traversable as T

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

tsIndex :: TileSet a -> TileIndex -> a
tsIndex ts i = case I.lookup i $ tileSet ts of
  Just a -> a
  Nothing -> error $ "key " ++ show i ++ " is not in TileSet"

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

gridSurrounding :: (Integral c) => Grid c a -> Coord c -> Surrounding a
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
  n = row +~ 1
  s = row +~ 1
  w = col +~ 1
  e = col +~ 1

-- }}}

-- TileMap {{{

data TileMap c = TileMap
  { _tmSize  :: Size c
  , _tileMap :: Grid c TileIndex
  } deriving (Eq,Show)

makeLenses ''TileMap

tmSurrounding :: (Integral c) => TileMap c -> Coord c -> Surrounding TileIndex
tmSurrounding = gridSurrounding . _tileMap

tmFromGrid :: (Integral c) => Grid c TileIndex -> TileMap c
tmFromGrid g = TileMap
  { _tmSize  = gridSize g
  , _tileMap = g
  }

tmOnGrid :: (Grid c TileIndex -> Grid c TileIndex) -> TileMap c -> TileMap c
tmOnGrid f = tileMap %~ f

tmOnGridA :: (Applicative f) => (Grid c TileIndex -> f (Grid c TileIndex))
  -> TileMap c -> f (TileMap c)
tmOnGridA f tm = TileMap (tm^.tmSize) <$> f (tm^.tileMap)


tmOnGridM :: (Monad m) => (Grid c TileIndex -> m (Grid c TileIndex))
  -> TileMap c -> m (TileMap c)
tmOnGridM f tm = do
  g <- f (tm^.tileMap)
  return $ tm { _tileMap = g }

mkEmptyTileMap :: (Integral c) => Size c -> TileMap c
mkEmptyTileMap sz = tmFromGrid $ mkEmptyGrid sz 0

tmSubMap :: (Ord c) => Coords c -> TileMap c -> TileMap c
tmSubMap = tmOnGrid . gridSubMap

tmSubMapByValue :: (Ord c) => TileIndex -> TileMap c -> Coords c
tmSubMapByValue ti = (() <$) . gridSubMapByValue ti . _tileMap

tmFromList :: (Integral c) => [(Coord c,TileIndex)] -> TileMap c
tmFromList = tmFromGrid . gridFromList

tmFilter :: (TileIndex -> Bool) -> TileMap c -> TileMap c
tmFilter = tmOnGrid . gridFilter

tmRows :: Integral c => TileMap c -> c
tmRows = gridRows . _tileMap

tmCols :: Integral c => TileMap c -> c
tmCols = gridCols . _tileMap

tmCoords :: Integral c => TileMap c -> [Coord c]
tmCoords = gridKeys . _tileMap

tmAssocs :: TileMap c -> [(Coord c,TileIndex)]
tmAssocs = gridContents . _tileMap

tmIndex :: (Ord c) => TileMap c -> Coord c -> TileIndex
tmIndex tm c = gridIndex (_tileMap tm) c

tmUpdate :: (TileIndex -> TileIndex) -> TileMap c -> TileMap c
tmUpdate = tmOnGrid . fmap

tmUpdateAt :: (Ord c) => [Coord c] -> (TileIndex -> TileIndex) -> TileMap c -> TileMap c
tmUpdateAt cs = tmOnGrid . gridUpdateAt cs

tmUpdateWithKeyAt :: (Ord c) => [Coord c] -> (Coord c -> TileIndex -> TileIndex)
  -> TileMap c -> TileMap c
tmUpdateWithKeyAt cs = tmOnGrid . gridUpdateWithKeyAt cs

tmUpdateAtM :: (Functor m, Monad m, Ord c) => [Coord c]
  -> (TileIndex -> m TileIndex) -> TileMap c -> m (TileMap c)
tmUpdateAtM cs = tmOnGridM . gridUpdateAtM cs

tmTraverseWithKey :: (Applicative m, Ord c) =>
  (Coord c -> TileIndex -> m TileIndex) -> TileMap c -> m (TileMap c)
tmTraverseWithKey = tmOnGridA . gridTraverseWithKey

tmUpdateWithKeyAtM :: (Functor m, Monad m, Ord c) => [Coord c]
  -> (Coord c -> TileIndex -> m TileIndex) -> TileMap c -> m (TileMap c)
tmUpdateWithKeyAtM cs = tmOnGridM . gridUpdateWithKeyAtM cs

tmTraverse :: (Applicative f, Ord c) => (TileIndex -> f TileIndex)
  -> TileMap c -> f (TileMap c)
tmTraverse = tmOnGridA . T.traverse

tmTraverseKeys :: (Applicative f, Ord c) => (Coord c -> f TileIndex)
  -> TileMap c -> f (TileMap c)
tmTraverseKeys = tmOnGridA . gridTraverseKeys

randomTileMap :: (Integral c) => (TileIndex,TileIndex)
  -> Size c -> Random (TileMap c)
randomTileMap rng sz = fmap (TileMap sz) . mkRandomGrid rng $ sz

{-
ppTileMap :: TileMap -> String
ppTileMap = ppGrid . tileMap
-}

csGenerateTileMap :: (Integral c) => (Coord c -> TileIndex)
  -> Coords c -> TileMap c
csGenerateTileMap f = tmFromGrid . gridMapKeysTo f

csGenerateTileMapA :: (Applicative m, Integral c) =>
  (Coord c -> m TileIndex) -> Coords c -> m (TileMap c)
csGenerateTileMapA f = fmap tmFromGrid . gridTraverseKeys f

-- }}}

