{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TileMap where

import Control.Monad.Trans.Random
import Data.Grid
import Data.Points
import Data.Surrounding
import Util

import Control.Applicative
import qualified Data.Traversable as T

newtype TileMap c = TileMap
  { tileMap :: Grid c TileIndex
  } deriving (Eq,Show)

-- Building {{{

tmFromGrid :: Grid c TileIndex -> TileMap c
tmFromGrid = TileMap

emptyTileMap :: TileMap c
emptyTileMap = tmFromGrid emptyGrid

mkRepeatTileMap :: (Integral c) => Size c -> TileMap c
mkRepeatTileMap sz = tmFromGrid $ mkRepeatGrid sz 0

mkIotaTileMapExcept :: (Integral c)
  => Size c -> [(Int,Int)] -> TileMap c
mkIotaTileMapExcept sz es = tmFromList $ flip zip [0..]
  $ concat
  $ deleteGridIndices es
  $ coordGrid sz

mkIotaTileMap :: (Integral c) => Size c -> TileMap c
mkIotaTileMap = tmFromGrid . mkIotaGrid

tmFromList :: (Integral c) => [(Coord c,TileIndex)] -> TileMap c
tmFromList = tmFromGrid . gridFromList

csGenerateTileMap :: (Integral c) => (Coord c -> TileIndex)
  -> Coords c -> TileMap c
csGenerateTileMap f = tmFromGrid . gridMapKeysTo f

csGenerateTileMapA :: (Applicative m, Integral c)
  => (Coord c -> m TileIndex) -> Coords c -> m (TileMap c)
csGenerateTileMapA f = fmap tmFromGrid . gridTraverseKeys f

tmInsert :: (Ord c) => Coord c -> TileIndex -> TileMap c -> TileMap c
tmInsert c = tmOnGrid . gridInsert c

-- }}}

-- Reducing {{{

tmSubMap :: (Ord c) => Coords c -> TileMap c -> TileMap c
tmSubMap = tmOnGrid . gridSubMap

tmSubMapByValue :: (Ord c) => TileIndex -> TileMap c -> Coords c
tmSubMapByValue ti = (() <$) . gridSubMapByValue ti . tileMap

tmFilter :: (TileIndex -> Bool) -> TileMap c -> TileMap c
tmFilter = tmOnGrid . gridFilter

tmDifference :: (Ord c) => TileMap c -> TileMap c -> TileMap c
tmDifference = tmOnGrid . gridDifference . tileMap

-- }}}

-- Mapping / Traversing {{{

tmOnGrid :: (Grid c TileIndex -> Grid c TileIndex) -> TileMap c -> TileMap c
tmOnGrid f tm = tm { tileMap = f $ tileMap tm }

tmOnGridA :: (Applicative f) => (Grid c TileIndex -> f (Grid c TileIndex))
  -> TileMap c -> f (TileMap c)
tmOnGridA f tm = TileMap <$> f (tileMap tm)

tmOnGridM :: (Monad m) => (Grid c TileIndex -> m (Grid c TileIndex))
  -> TileMap c -> m (TileMap c)
tmOnGridM f tm = do
  g <- f (tileMap tm)
  return $ tm { tileMap = g }

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

tmTraverseWithKey :: (Applicative m, Ord c)
  => (Coord c -> TileIndex -> m TileIndex) -> TileMap c -> m (TileMap c)
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

-- }}}

-- Accessing {{{

tmRows :: Integral c => TileMap c -> c
tmRows = gridRows . tileMap

tmCols :: Integral c => TileMap c -> c
tmCols = gridCols . tileMap

tmCoords :: Integral c => TileMap c -> [Coord c]
tmCoords = gridKeys . tileMap

tmAssocs :: TileMap c -> [(Coord c,TileIndex)]
tmAssocs = gridContents . tileMap

tmIndex :: (Ord c) => TileMap c -> Coord c -> TileIndex
tmIndex tm c = gridIndex (tileMap tm) c

tmLookup :: (Ord c) => TileMap c -> Coord c -> Maybe TileIndex
tmLookup tm c = gridLookup (tileMap tm) c

tmMinimum, tmMaximum :: (Ord c) => TileMap c -> Maybe (Coord c, TileIndex)
tmMinimum = gridMinimum . tileMap
tmMaximum = gridMaximum . tileMap

tmSize :: (Integral c) => TileMap c -> Size c
tmSize = gridSize . tileMap

-- }}}

-- Random {{{

randomTileMap :: (Integral c) => (TileIndex,TileIndex)
  -> Size c -> Random (TileMap c)
randomTileMap rng sz = fmap TileMap . mkRandomGrid rng $ sz

-- }}}

-- Surrounding {{{

tmSurrounding :: (Integral c) => TileMap c -> Coord c -> Surrounding TileIndex
tmSurrounding = gridSurrounding . tileMap

-- }}}

-- Pretty Printing {{{

printTileMap :: (Integral c, Show c) => TileMap c -> IO ()
printTileMap = putStrLn . ppTileMap

ppTileMap :: (Integral c, Show c) => TileMap c -> String
ppTileMap = ppGrid . tileMap

-- }}}

