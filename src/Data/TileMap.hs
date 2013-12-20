{-# LANGUAGE OverloadedStrings #-}

module Data.TileMap where

import Data.Grid
import Data.Points
import Data.Surrounding
import Data.TileSet
import Error
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

mkRepeatTileMap :: (DiscreteCoord c) => Size c -> TileMap c
mkRepeatTileMap sz = tmFromGrid $ mkRepeatGrid sz 0

mkIotaTileMapExcept :: (CoordType c)
  => Size c -> [(Int,Int)] -> TileMap c
mkIotaTileMapExcept sz es = tmFromList $ flip zip [0..]
  $ concat
  $ deleteGridIndices es
  $ coordGrid sz

mkIotaTileMap :: (DiscreteCoord c) => Size c -> TileMap c
mkIotaTileMap = tmFromGrid . mkIotaGrid

tmFromList :: (CoordType c) => [(Coord c,TileIndex)] -> TileMap c
tmFromList = tmFromGrid . gridFromList

csGenerateTileMap :: (CoordType c) => (Coord c -> TileIndex)
  -> Coords c -> TileMap c
csGenerateTileMap f = tmFromGrid . gridMapKeysTo f

csGenerateTileMapA :: (Applicative m, CoordType c)
  => (Coord c -> m TileIndex) -> Coords c -> m (TileMap c)
csGenerateTileMapA f = fmap tmFromGrid . gridTraverseKeys f

tmInsert :: (CoordType c) => Coord c -> TileIndex -> TileMap c -> TileMap c
tmInsert c = tmOnGrid . gridInsert c

tmUnion :: (CoordType c) => TileMap c -> TileMap c -> TileMap c
tmUnion m1 = tmFromGrid . gridUnion (tileMap m1) . tileMap

tmUnions :: (CoordType c) => [TileMap c] -> TileMap c
tmUnions = tmFromGrid . gridUnions . map tileMap

-- }}}

-- Reducing {{{

tmFilter :: (TileIndex -> Bool) -> TileMap c -> TileMap c
tmFilter = tmOnGrid . gridFilter

tmDifference :: (CoordType c) => TileMap c -> TileMap c -> TileMap c
tmDifference = tmOnGrid . gridDifference . tileMap


newtype SubMap c = SubMap
  { subMap :: TileMap c
  } deriving (Eq,Show)

tmSubMap :: (CoordType c) => Coords c -> TileMap c -> SubMap c
tmSubMap cs = SubMap . tmOnGrid (gridSubMap cs)

tmSubMapByIndex :: (CoordType c) => TileIndex -> TileMap c -> SubMap c
tmSubMapByIndex ti = SubMap . tmOnGrid (gridSubMapByValue ti)

tmSubMapsByIndex :: (CoordType c) => TileMap c -> [SubMap c]
tmSubMapsByIndex tm = map (`tmSubMapByIndex` tm) $ tmValues tm

tmSubMapSetByIndex :: (CoordType c) => TileMap c -> TileSet (SubMap c)
tmSubMapSetByIndex tm = tsFromList [ (i,tmSubMapByIndex i tm) | i <- is ]
  where
  is = tmValues tm

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

tmUpdateAt :: (CoordType c) => [Coord c] -> (TileIndex -> TileIndex) -> TileMap c -> TileMap c
tmUpdateAt cs = tmOnGrid . gridUpdateAt cs

tmUpdateWithKeyAt :: (CoordType c) => [Coord c] -> (Coord c -> TileIndex -> TileIndex)
  -> TileMap c -> TileMap c
tmUpdateWithKeyAt cs = tmOnGrid . gridUpdateWithKeyAt cs

tmUpdateAtM :: (Functor m, Monad m, CoordType c) => [Coord c]
  -> (TileIndex -> m TileIndex) -> TileMap c -> m (TileMap c)
tmUpdateAtM cs = tmOnGridM . gridUpdateAtM cs

tmTraverseWithKey :: (Applicative m, CoordType c)
  => (Coord c -> TileIndex -> m TileIndex) -> TileMap c -> m (TileMap c)
tmTraverseWithKey = tmOnGridA . gridTraverseWithKey

tmUpdateWithKeyAtM :: (Functor m, Monad m, CoordType c) => [Coord c]
  -> (Coord c -> TileIndex -> m TileIndex) -> TileMap c -> m (TileMap c)
tmUpdateWithKeyAtM cs = tmOnGridM . gridUpdateWithKeyAtM cs

tmTraverse :: (Applicative f, CoordType c) => (TileIndex -> f TileIndex)
  -> TileMap c -> f (TileMap c)
tmTraverse = tmOnGridA . T.traverse

tmTraverseKeys :: (Applicative f, CoordType c) => (Coord c -> f TileIndex)
  -> TileMap c -> f (TileMap c)
tmTraverseKeys = tmOnGridA . gridTraverseKeys

tmFoldrWithKey :: (Coord c -> TileIndex -> a -> a) -> a -> TileMap c -> a
tmFoldrWithKey f a = gridFoldrWithKey f a . tileMap

tmFoldrKeys :: (Coord c -> a -> a) -> a -> TileMap c -> a
tmFoldrKeys f a = gridFoldrKeys f a . tileMap

-- }}}

-- Accessing {{{

tmRows :: CoordType c => TileMap c -> c
tmRows = gridRows . tileMap

tmCols :: CoordType c => TileMap c -> c
tmCols = gridCols . tileMap

tmCoords :: CoordType c => TileMap c -> [Coord c]
tmCoords = gridKeys . tileMap

tmValues :: CoordType c => TileMap c -> [TileIndex]
tmValues = gridValues . tileMap

tmAssocs :: TileMap c -> [(Coord c,TileIndex)]
tmAssocs = gridContents . tileMap

tmIndex :: (CoordType c) => TileMap c -> Coord c -> TileIndex
tmIndex tm = gridIndex (tileMap tm)

tmLookup :: (CoordType c) => TileMap c -> Coord c -> Error TileIndex
tmLookup tm c = reportNothing err $ gridLookup (tileMap tm) c
  where
  err = "Couldn't find coord " ++ ppCoord c ++ "in TileMap:\n" ++ ppTileMap tm

tmMinimum, tmMaximum :: (CoordType c) => TileMap c -> Error (Coord c, TileIndex)
tmMinimum tm = reportNothing "tmMinimum: empty TileMap" $ gridMinimum $ tileMap tm
tmMaximum tm = reportNothing "tmMaximum: empty TileMap" $ gridMaximum $ tileMap tm

tmSize :: (CoordType c) => TileMap c -> Size c
tmSize = gridSize . tileMap

-- }}}

-- Surrounding {{{

tmSurrounding :: (CoordType c) => TileMap c -> Coord c -> Surrounding TileIndex
tmSurrounding = gridSurrounding . tileMap

-- }}}

-- Pretty Printing {{{

printTileMap :: (CoordType c, Show c) => TileMap c -> IO ()
printTileMap = putStrLn . ppTileMap

ppTileMap :: (CoordType c, Show c) => TileMap c -> String
ppTileMap = ppGrid . tileMap

-- }}}

