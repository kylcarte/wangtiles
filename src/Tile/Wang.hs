{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Tile.Wang where

import Control.Monad
import qualified Data.IntMap as I
import qualified Data.Set as S
import qualified Data.Array as A
import qualified System.Random as R
import System.Random (RandomGen(..))

import Graphics.Gloss.Data.Picture
import Tile
import Util

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

data Constraint
  = ConstrainEdge Edge Color
  deriving (Eq,Ord,Show)

type Constraints = S.Set Constraint

colorConstraints :: [(Edge,Color)] -> Constraints
colorConstraints = S.fromList . map (uncurry ConstrainEdge)

type WangTileSet = I.IntMap (Picture,Tile)

data Tile = Tile
  { tColors  :: A.Array Edge Color
  } deriving (Eq,Show)

mkTile :: Color -> Color -> Color -> Color -> Tile
mkTile n s w e = Tile
  { tColors = A.array (minBound,maxBound)
      [ ( North , n )
      , ( South , s )
      , ( West  , w )
      , ( East  , e )
      ]
  }

edgeColor :: Tile -> Edge -> Color
edgeColor t e = tColors t A.! e

satisfies :: Constraints -> Tile -> Bool
satisfies cs t = S.foldl sat True cs
  where
  sat False _ = False
  sat _ (ConstrainEdge e c) = edgeColor t e == c

selectTiles :: Constraints -> WangTileSet -> WangTileSet
selectTiles cs ts = I.fromList suitables
  where
  allTiles  = I.assocs ts
  suitables = filter sat allTiles
  sat (_,(_,t)) = satisfies cs t

textureIn :: WangTileSet -> TileIndex -> Picture
textureIn ts i = fst $ tileTexture ts i

tileIn :: WangTileSet -> TileIndex -> Tile
tileIn ts i = snd $ tileTexture ts i

tileTexture :: WangTileSet -> TileIndex -> (Picture,Tile)
tileTexture ts i = ts I.! i

edgeColorAt :: WangTileSet -> Coord -> Edge -> TileMap -> Color
edgeColorAt ts c e tm = edgeColor t e
  where
  ti = tm ! c
  t  = tileIn ts ti

updateTile :: WangTileSet -> TileMap -> Coord -> Random TileMap
updateTile ts tm c@(row,col) = do
  i <- randomTileIndex suitable
  return $ update c i tm
  where
  leftOf = (row,col-1)
  above  = (row-1,col)
  suitable = selectTiles cs ts
  cs = colorConstraints csl
  csl = concat
         [ if row == 0 then [] else [ ( North , edgeColorAt ts above  South tm ) ]
         , if col == 0 then [] else [ ( West  , edgeColorAt ts leftOf East  tm ) ]
         ]

ioWangTileMap :: WangTileSet -> Size -> IO TileMap
ioWangTileMap ts sz = do
  g <- R.getStdGen
  let (tm,g') = mkWangTileMap ts sz g
  R.setStdGen g'
  return tm

mkWangTileMap :: RandomGen g => WangTileSet -> Size -> g -> (TileMap,g)
mkWangTileMap ts (r,c) g = runRandom g $ randomWangTileMap ts (r-1,c-1)

randomWangTileMap :: WangTileSet -> Size -> Random TileMap
randomWangTileMap ts sz@(rs,cs) = foldM (updateTile ts) initialTm coords
  where
  coords = [ (r,c) | r <- [0..rs], c <- [0..cs] ]
  initialTm = mkDefaultTileMap sz

randomTileIndex :: WangTileSet -> Random TileIndex
randomTileIndex = randomKey

wangTiles2x2 :: [Tile]
wangTiles2x2 =
  [ mkTile Red   Green Blue   Yellow
  , mkTile Green Green Blue   Blue  
  , mkTile Red   Red   Yellow Yellow
  , mkTile Green Red   Yellow Blue  
  , mkTile Red   Green Yellow Blue  
  , mkTile Green Green Yellow Yellow
  , mkTile Red   Red   Blue   Blue  
  , mkTile Green Red   Blue   Yellow
  ]


