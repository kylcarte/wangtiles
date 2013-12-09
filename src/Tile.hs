{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Tile where

import qualified Data.IntMap as I
import qualified Data.Array as A

type Coord = (Int,Int)
type Size  = (Int,Int)

newtype Grid a = Grid
  { fromGrid :: A.Array Coord a
  } deriving (Eq,Show)

gridSize :: Grid a -> Size
gridSize g = (rMax + 1,cMax + 1)
  where
  (rMax,cMax) = snd . A.bounds . fromGrid $ g

rows :: Grid a -> Int
rows = fst . gridSize

cols :: Grid a -> Int
cols = snd . gridSize

gridContents :: Grid a -> [(Coord,a)]
gridContents = A.assocs . fromGrid

type TileIndex = Int

{-
newtype TileIndex = TileIndex
  { tileIndex :: Int
  } deriving (Eq,Ord,Show,A.Ix,Random)
-}

type TileSet = I.IntMap

{-
data TileSet a = TileSet
  { tileSet :: M.Map TileIndex a
  } deriving (Eq,Show)
-}

data TileMap = TileMap
  { tileMap :: Grid TileIndex
  } deriving (Eq,Show)

(!) :: TileMap -> Coord -> TileIndex
tm ! i = fromGrid (tileMap tm) A.! i

(//) :: TileMap -> [(Coord,TileIndex)] -> TileMap
tm // ts = tm { tileMap = Grid $ fromGrid (tileMap tm) A.// ts }

update :: Coord -> TileIndex -> TileMap -> TileMap
update xy t tm = tm // [(xy,t)]

mkDefaultTileMap :: Size -> TileMap
mkDefaultTileMap sz = TileMap 
  { tileMap = Grid
      $ A.listArray ((0,0),sz)
      $ repeat 0
  }

fromList :: (A.Ix i) => [(i,e)] -> A.Array i e
fromList es = A.array (lo,hi) es
  where
  ixs = map fst es
  lo = minimum ixs
  hi = maximum ixs

