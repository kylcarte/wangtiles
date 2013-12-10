{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Tile.Wang where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import qualified System.Random as R
import System.Random (RandomGen(..))

import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Color as Gloss
import Tile
import Util

-- Tiles {{{

wangTiles2x2 :: TileSet Tile
wangTiles2x2 = mkTiles
  [ ( Red   , Green , Blue   , Yellow )
  , ( Green , Green , Blue   , Blue   )
  , ( Red   , Red   , Yellow , Yellow )
  , ( Green , Red   , Yellow , Blue   )
  , ( Red   , Green , Yellow , Blue   )
  , ( Green , Green , Yellow , Yellow )
  , ( Red   , Red   , Blue   , Blue   )
  , ( Green , Red   , Blue   , Yellow )
  ]

wangTiles2x3 :: TileSet Tile
wangTiles2x3 = mkTiles
  [ ( Red   , Green , Blue   , Yellow )
  , ( Green , Red   , Blue   , Yellow )
  , ( Red   , Red   , Yellow , Blue   )
  , ( Green , Green , Yellow , Red    )
  , ( Red   , Green , Red    , Yellow )
  , ( Green , Red   , Red    , Blue   )
  , ( Red   , Green , Blue   , Red    )
  , ( Red   , Red   , Blue   , Red    )
  , ( Green , Red   , Yellow , Red    )
  , ( Red   , Green , Yellow , Blue   )
  , ( Green , Green , Red    , Blue   )
  , ( Green , Red   , Red    , Yellow )
  ]

-- }}}

-- Tile {{{

data Tile = Tile
  { tColors :: M.Map Edge Color
  } deriving (Eq,Show)

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
  deriving (Eq,Ord,Show,Enum,Bounded)

mkTile :: Color -> Color -> Color -> Color -> Tile
mkTile n s w e = Tile $ M.fromList
  [ ( North , n )
  , ( South , s )
  , ( West  , w )
  , ( East  , e )
  ]

mkTiles :: [(Color,Color,Color,Color)] -> TileSet Tile
mkTiles = tsFromList . zip [0..] . map (uncurry4 mkTile)

tileColors :: Tile -> [(Edge,Color)]
tileColors = M.assocs . tColors

edgeColor :: Tile -> Edge -> Color
edgeColor t e = tColors t M.! e

edgeAxis :: a -> a -> Edge -> a
edgeAxis ns we e = case e of
  North -> ns
  South -> ns
  West  -> we
  East  -> we

glossColor :: Color -> Gloss.Color
glossColor c = case c of
  Red    -> Gloss.red
  Green  -> Gloss.green
  Yellow -> Gloss.yellow
  Blue   -> Gloss.blue

-- }}}

-- Constraint {{{

data Constraint
  = ConstrainEdge Edge Color
  deriving (Eq,Ord,Show)

type Constraints = S.Set Constraint

colorConstraints :: [(Edge,Color)] -> Constraints
colorConstraints = S.fromList . map (uncurry ConstrainEdge)

satisfies :: Constraints -> Tile -> Bool
satisfies cs t = S.foldl sat True cs
  where
  sat False _ = False
  sat _ (ConstrainEdge e c) = edgeColor t e == c

selectTiles :: Constraints -> WangTileSet -> WangTileSet
selectTiles cs ts = tsFromList suitables
  where
  allTiles  = tsAssocs ts
  suitables = filter sat allTiles
  sat (_,(_,t)) = satisfies cs t

-- }}}

-- WangTileSet {{{

type WangTileSet = TileSet (Picture,Tile)

textureIn :: WangTileSet -> TileIndex -> Picture
textureIn ts i = fst $ tileTexture ts i

tileIn :: WangTileSet -> TileIndex -> Tile
tileIn ts i = snd $ tileTexture ts i

tileTexture :: WangTileSet -> TileIndex -> (Picture,Tile)
tileTexture ts i = tsIndex ts i

randomTileIndex :: WangTileSet -> Random TileIndex
randomTileIndex = tsRandomKey

-- }}}

-- TileMap {{{

edgeColorAt :: WangTileSet -> Coord -> Edge -> TileMap -> Color
edgeColorAt ts c e tm = edgeColor t e
  where
  ti = tmIndex tm c
  t  = tileIn ts ti

updateTile :: WangTileSet -> TileMap -> Coord -> Random TileMap
updateTile ts tm c@(row,col) = do
  i <- randomTileIndex suitable
  return $ tmUpdate1 c i tm
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
mkWangTileMap ts sz g = runRandom g $ randomWangTileMap ts sz

randomWangTileMap :: WangTileSet -> Size -> Random TileMap
randomWangTileMap ts sz@(rs,cs) = foldM (updateTile ts) initialTm coords
  where
  coords = [ (r,c) | r <- [0..rs], c <- [0..cs] ]
  initialTm = mkEmptyTileMap sz

-- }}}

