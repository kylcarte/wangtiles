{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Tile where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Default
import qualified Data.Map as M
import qualified Data.Array as A
import qualified System.Random as R
import System.Random (RandomGen(..),StdGen(..))

data Color
  = Red
  | Green
  | Yellow
  | Blue
  deriving (Eq,Ord,Show)

data Edge
  = North
  | South
  | West
  | East
  deriving (Eq,Ord,Show,A.Ix,Enum,Bounded)

type Constraints = M.Map Edge Color

constraints :: [(Edge,Color)] -> Constraints
constraints = M.fromList

data Tile = Tile
  { tColors :: A.Array Edge Color
  } deriving (Eq,Show)

mkTile :: Color -> Color -> Color -> Color -> Tile
mkTile n s w e = Tile $ A.array (minBound,maxBound)
  [ ( North , n )
  , ( South , s )
  , ( West  , w )
  , ( East  , e )
  ]

edgeColor :: Tile -> Edge -> Color
edgeColor t e = tColors t A.! e

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

newtype TileIndex = TileIndex
  { tileIndex :: Int
  } deriving (Eq,Ord,Show,A.Ix,R.Random,Default)

newtype TileSet = TileSet
  { fromTileSet :: M.Map TileIndex Tile
  } deriving (Eq,Show)

data TileMap = TileMap
  { tileMap :: Grid TileIndex
  , tileSet :: TileSet
  } deriving (Eq,Show)

satisfies :: Constraints -> Tile -> Bool
satisfies cs t = M.foldlWithKey f True cs
  where
  f False _ _ = False
  f _ e c     = edgeColor t e == c

selectTiles :: Constraints -> TileSet -> TileSet
selectTiles cs ts = TileSet $ M.fromList suitables
  where
  allTiles  = M.assocs $ fromTileSet ts
  suitables = filter (satisfies cs . snd) allTiles

(!) :: TileMap -> Coord -> TileIndex
tm ! i = fromGrid (tileMap tm) A.! i

tileIn :: TileSet -> TileIndex -> Tile
tileIn ts i = fromTileSet ts M.! i

(//) :: TileMap -> [(Coord,TileIndex)] -> TileMap
tm // ts = tm { tileMap = Grid $ fromGrid (tileMap tm) A.// ts }

update :: Coord -> TileIndex -> TileMap -> TileMap
update xy t tm = tm // [(xy,t)]

instance Default TileSet where
  def = TileSet
    $ M.fromList $ zip [ TileIndex i | i <- [0..7] ]
    [ mkTile Red   Green Blue   Yellow
    , mkTile Green Green Blue   Blue  
    , mkTile Red   Red   Yellow Yellow
    , mkTile Green Red   Yellow Blue  
    , mkTile Red   Green Yellow Blue  
    , mkTile Green Green Yellow Yellow
    , mkTile Red   Red   Blue   Blue  
    , mkTile Green Red   Blue   Yellow
    ]

mkDefaultTileMap :: TileSet -> Size -> TileMap
mkDefaultTileMap ts sz = TileMap 
  { tileMap = Grid
      $ A.listArray ((0,0),sz)
      $ repeat def
  , tileSet = ts
  }

fromList :: (A.Ix i) => [(i,e)] -> A.Array i e
fromList es = A.array (lo,hi) es
  where
  ixs = map fst es
  lo = minimum ixs
  hi = maximum ixs

edgeColorAt :: Coord -> Edge -> TileMap -> Color
edgeColorAt c e tm = edgeColor t e
  where
  ti = tm ! c
  t  = tileIn (tileSet tm) ti

updateTile :: TileMap -> Coord -> Random TileMap
updateTile tm c@(row,col) = do
  -- msg $ "Constraints of " ++ show c ++ " are " ++ show csl
  -- msg $ "Possible tiles are " ++ show suitable
  i <- randomTileIndex suitable
  -- msg $ "Updating tile to " ++ show (tileIn ts i)
  -- msg ""
  return $ update c i tm
  where
  leftOf = (row,col-1)
  above  = (row-1,col)
  ts = tileSet tm
  suitable = selectTiles cs ts
  cs = constraints csl
  csl = concat
         [ if row == 0 then [] else [ ( North , edgeColorAt above  South tm ) ]
         , if col == 0 then [] else [ ( West  , edgeColorAt leftOf East  tm ) ]
         ]

ioWangTileMap :: TileSet -> Size -> IO TileMap
ioWangTileMap ts sz = do
  g <- R.getStdGen
  let ((tm,msgs),g') = mkWangTileMap ts sz g
  R.setStdGen g'
  mapM_ putStrLn msgs
  return tm

mkWangTileMap :: RandomGen g => TileSet -> Size -> g -> ((TileMap,[String]),g)
mkWangTileMap ts (r,c) g = runRandom g $ randomWangTileMap ts (r-1,c-1)

randomWangTileMap :: TileSet -> Size -> Random TileMap
randomWangTileMap ts sz@(rs,cs) = foldM updateTile initialTm coords
  where
  coords = [ (r,c) | r <- [0..rs], c <- [0..cs] ]
  initialTm = mkDefaultTileMap ts sz

-- Random {{{

newtype Random a = Random
  { unRandom :: RandomGen g => WriterT [String] (State g) a
  }

instance Functor Random where
  fmap f (Random m) = Random $ fmap f m

instance Applicative Random where
  pure = return
  (<*>) = ap

instance Monad Random where
  return a = Random $ lift $ state $ \g -> (a,g)
  (Random m) >>= f = Random $ m >>= unRandom . f

runRandom :: RandomGen g => g -> Random a -> ((a,[String]),g)
runRandom g m = runState (runWriterT $ unRandom m) g

-- Key functions

randomTile :: TileSet -> Random Tile
randomTile = randomVal . fromTileSet

randomTileIndex :: TileSet -> Random TileIndex
randomTileIndex = randomKey . fromTileSet

randomVal :: (Ord k) => M.Map k a -> Random a
randomVal m = do
  k <- randomKey m
  return $ m M.! k

randomKey :: (Ord k) => M.Map k a -> Random k
randomKey m = rnd $ first (ks !!) . R.randomR (0,n)
  where
  ks = M.keys m
  n = length ks - 1

rnd :: (forall g. RandomGen g => g -> (a,g)) -> Random a
rnd f = Random $ lift $ state f

msg :: String -> Random ()
msg s = Random $ tell [s]

-- }}}

