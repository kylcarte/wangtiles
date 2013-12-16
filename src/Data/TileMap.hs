{-# LANGUAGE TemplateHaskell #-}

module Data.TileMap where

import Control.Monad.Trans.Random
import Data.Grid
import Data.Points
import Data.Surrounding
import Util

import Control.Applicative
import Control.Lens
import qualified Data.Traversable as T

newtype TileMap c = TileMap
  { _tileMap :: Grid c TileIndex
  } deriving (Eq,Show)

makeLenses ''TileMap

tmSurrounding :: (Integral c) => TileMap c -> Coord c -> Surrounding TileIndex
tmSurrounding = gridSurrounding . _tileMap

tmFromGrid :: (Integral c) => Grid c TileIndex -> TileMap c
tmFromGrid = TileMap

tmOnGrid :: (Grid c TileIndex -> Grid c TileIndex) -> TileMap c -> TileMap c
tmOnGrid f = tileMap %~ f

tmOnGridA :: (Applicative f) => (Grid c TileIndex -> f (Grid c TileIndex))
  -> TileMap c -> f (TileMap c)
tmOnGridA f tm = TileMap <$> f (tm^.tileMap)


tmOnGridM :: (Monad m) => (Grid c TileIndex -> m (Grid c TileIndex))
  -> TileMap c -> m (TileMap c)
tmOnGridM f tm = do
  g <- f (tm^.tileMap)
  return $ tm { _tileMap = g }

mkEmptyTileMap :: (Integral c) => Size c -> TileMap c
mkEmptyTileMap sz = tmFromGrid $ mkEmptyGrid sz 0

mkIotaTileMap :: (Integral c) => Size c -> TileMap c
mkIotaTileMap = tmFromGrid . mkIotaGrid

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

tmMinimum, tmMaximum :: (Ord c) => TileMap c -> Maybe (Coord c, TileIndex)
tmMinimum = gridMinimum . _tileMap
tmMaximum = gridMaximum . _tileMap

tmDifference :: (Ord c) => TileMap c -> TileMap c -> TileMap c
tmDifference = tmOnGrid . gridDifference . _tileMap

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
randomTileMap rng sz = fmap TileMap . mkRandomGrid rng $ sz

ppTileMap :: (Integral c, Show c) => TileMap c -> String
ppTileMap = ppGrid . _tileMap

csGenerateTileMap :: (Integral c) => (Coord c -> TileIndex)
  -> Coords c -> TileMap c
csGenerateTileMap f = tmFromGrid . gridMapKeysTo f

csGenerateTileMapA :: (Applicative m, Integral c) =>
  (Coord c -> m TileIndex) -> Coords c -> m (TileMap c)
csGenerateTileMapA f = fmap tmFromGrid . gridTraverseKeys f

