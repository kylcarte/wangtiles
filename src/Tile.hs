{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Tile where

import Util

import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified System.Random as R

-- Coords {{{

newtype Coords = Coords
  { coords :: Grid ()
  } deriving (Eq,Show)

csMap :: (Coord -> a) -> Coords -> Grid a
csMap f = gridOnMap (M.mapWithKey (const . f)) . coords

csTraverse :: (Applicative m) => (Coord -> m a) -> Coords -> m (Grid a)
csTraverse f = gridTraverseKeys f . coords

csGenerateTileMap :: (Coord -> TileIndex) -> Coords -> TileMap
csGenerateTileMap f = tmFromGrid . csMap f

csGenerateTileMapA :: (Applicative m) => (Coord -> m TileIndex) -> Coords -> m TileMap
csGenerateTileMapA f = fmap tmFromGrid . csTraverse f

coordIsIn :: Coords -> Coord -> Bool
coordIsIn cs c = gridMember c $ coords cs

-- }}}

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
mkEmptyGrid sz a = Grid $ M.fromList
  [ (Coord { row = r , col = c },a)
  | c <- [0..width  sz - 1]
  , r <- [0..height sz - 1]
  ]

gridSubMap :: Coords -> Grid a -> Grid a
gridSubMap cs = gridOnMap $ M.filterWithKey fn
  where
  fn c _ = coordIsIn cs c

gridSubMapByValue :: Eq a => a -> Grid a -> Grid a
gridSubMapByValue = gridOnMap . M.filter . (==)

gridFromList :: [(Coord,a)] -> Grid a
gridFromList = Grid . M.fromList

gridSize :: Grid a -> Size
gridSize = mkSize . getMaxs
  where
  mkSize (mr,mc) = Size { width = mc , height = mr }
  getMaxs = 
      onPair (succ . maximum)
    . colsRows
    . M.keys
    . fromGrid

gridRows :: Grid a -> Int
gridRows = height . gridSize

gridCols :: Grid a -> Int
gridCols = width . gridSize

gridCoords :: Grid a -> [Coord]
gridCoords = M.keys . fromGrid

gridFilter :: (a -> Bool) -> Grid a -> Grid a
gridFilter = gridOnMap . M.filter

gridMember :: Coord -> Grid a -> Bool
gridMember c = M.member c . fromGrid

gridIndex :: Grid a -> Coord -> a
gridIndex g c = fromGrid g M.! c

gridLookup :: Grid a -> Coord -> Maybe a
gridLookup g c = M.lookup c (fromGrid g)

gridContents :: Grid a -> [(Coord,a)]
gridContents = M.assocs . fromGrid

gridOnMap :: (M.Map Coord a -> M.Map Coord b) -> Grid a -> Grid b
gridOnMap f = Grid . f . fromGrid

gridOnMapM :: (Functor m) => (M.Map Coord a -> m (M.Map Coord b)) -> Grid a -> m (Grid b)
gridOnMapM f = fmap Grid . f . fromGrid

gridUpdateWithKey :: (Coord -> a -> a) -> Grid a -> Grid a
gridUpdateWithKey = gridOnMap . M.mapWithKey

gridUpdateAt :: [Coord] -> (a -> a) -> Grid a -> Grid a
gridUpdateAt cs = gridOnMap . mapUpdateAt cs

gridUpdateWithKeyAt :: [Coord] -> (Coord -> a -> a)
  -> Grid a -> Grid a
gridUpdateWithKeyAt cs = gridOnMap . mapUpdateWithKeyAt cs

gridUpdateAtM :: (Functor m, Monad m) => [Coord]
  -> (a -> m a) -> Grid a -> m (Grid a)
gridUpdateAtM cs = gridOnMapM . mapUpdateAtM cs

gridUpdateWithKeyAtM :: (Functor m, Monad m) => [Coord]
  -> (Coord -> a -> m a) -> Grid a -> m (Grid a)
gridUpdateWithKeyAtM cs = gridOnMapM . mapUpdateWithKeyAtM cs

gridTraverseWithKey :: (Functor m, Applicative m) => (Coord -> a -> m a)
  -> Grid a -> m (Grid a)
gridTraverseWithKey = gridOnMapM . M.traverseWithKey

gridTraverseKeys :: (Functor m, Applicative m) => (Coord -> m a) -> Grid b -> m (Grid a)
gridTraverseKeys f = gridOnMapM $ M.traverseWithKey (const . f)

randomGrid :: forall a. R.Random a => (a,a) -> Size -> Random (Grid a)
randomGrid rng sz = T.traverse f . mkEmptyGrid sz =<< random
  where
  f :: a -> Random a
  f = const $ randomR rng

ppGrid :: Show a => Grid a -> String
ppGrid g = ppRows
  [ [ pad s
    | c <- [0..width sz - 1]
    , let s = fromMaybe " " $ fmap show $ gridLookup g Coord { row = r , col = c }
    ]
  | r <- [0..height sz - 1]
  ]
  where
  sz = gridSize g
  rdigits = logBase 10 `withFloat` width sz
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
  n = onVertical   pred
  s = onVertical   succ
  w = onHorizontal pred
  e = onHorizontal succ

tmSurrounding :: TileMap -> Coord -> Surrounding TileIndex
tmSurrounding = gridSurrounding . tileMap

-- }}}

-- TileMap {{{

data TileMap = TileMap
  { tmSize  :: Size
  , tileMap :: Grid TileIndex
  } deriving (Eq,Show)

tmFromGrid :: Grid TileIndex -> TileMap
tmFromGrid g = TileMap { tmSize  = gridSize g , tileMap = g }

mkEmptyTileMap :: Size -> TileMap
mkEmptyTileMap sz = tmFromGrid $ mkEmptyGrid sz 0

tmSubMap :: Coords -> TileMap -> TileMap
tmSubMap = tmOnGrid . gridSubMap

tmSubMapByValue :: TileIndex -> TileMap -> Coords
tmSubMapByValue ti = Coords . (() <$) . gridSubMapByValue ti . tileMap

tmFromList :: [(Coord,TileIndex)] -> TileMap
tmFromList = tmFromGrid . gridFromList

tmFilter :: (TileIndex -> Bool) -> TileMap -> TileMap
tmFilter = tmOnGrid . gridFilter

{-
tmSize :: TileMap -> Size
tmSize = gridSize . tileMap
-}

tmRows :: TileMap -> Int
tmRows = gridRows . tileMap

tmCols :: TileMap -> Int
tmCols = gridCols . tileMap

tmCoords :: TileMap -> [Coord]
tmCoords = gridCoords . tileMap

tmAssocs :: TileMap -> [(Coord,TileIndex)]
tmAssocs = gridContents . tileMap

tmIndex :: TileMap -> Coord -> TileIndex
tmIndex tm c = gridIndex (tileMap tm) c

tmOnGrid :: (Grid TileIndex -> Grid TileIndex) -> TileMap -> TileMap
tmOnGrid f tm = TileMap (tmSize tm) . f . tileMap $ tm

tmOnGridM :: (Functor m) => (Grid TileIndex -> m (Grid TileIndex)) -> TileMap -> m TileMap
tmOnGridM f tm = fmap (TileMap $ tmSize tm) . f . tileMap $ tm

tmUpdate :: (TileIndex -> TileIndex) -> TileMap -> TileMap
tmUpdate = tmOnGrid . fmap

tmUpdateWithKey :: (Coord -> TileIndex -> TileIndex)
  -> TileMap -> TileMap
tmUpdateWithKey = tmOnGrid . gridUpdateWithKey

tmUpdateAt :: [Coord] -> (TileIndex -> TileIndex) -> TileMap -> TileMap
tmUpdateAt cs = tmOnGrid . gridUpdateAt cs

tmUpdateWithKeyAt :: [Coord] -> (Coord -> TileIndex -> TileIndex)
  -> TileMap -> TileMap
tmUpdateWithKeyAt cs = tmOnGrid . gridUpdateWithKeyAt cs

tmUpdateAtM :: (Functor m, Monad m) => [Coord]
  -> (TileIndex -> m TileIndex) -> TileMap -> m TileMap
tmUpdateAtM cs = tmOnGridM . gridUpdateAtM cs

tmTraverseWithKey :: (Applicative m) => (Coord -> TileIndex -> m TileIndex)
  -> TileMap -> m TileMap
tmTraverseWithKey = tmOnGridM . gridTraverseWithKey

tmUpdateWithKeyAtM :: (Functor m, Monad m) => [Coord]
  -> (Coord -> TileIndex -> m TileIndex) -> TileMap -> m TileMap
tmUpdateWithKeyAtM cs = tmOnGridM . gridUpdateWithKeyAtM cs

tmTraverse :: (Applicative f) => (TileIndex -> f TileIndex)
  -> TileMap -> f TileMap
tmTraverse = tmOnGridM . T.traverse

randomTileMap :: (TileIndex,TileIndex) -> Size -> Random TileMap
randomTileMap rng sz = fmap (TileMap sz) . randomGrid rng $ sz

ppTileMap :: TileMap -> String
ppTileMap = ppGrid . tileMap

-- }}}

