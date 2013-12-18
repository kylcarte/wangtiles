{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Tile.Wang where

import Control.Monad.Random
import Data.Points
import Data.TileMap
import Data.TileSet
import Tile
import Util

import Control.Applicative
import Control.Lens
import Data.List (delete)
import qualified Data.Map as M
import qualified Graphics.Gloss.Data.Color as Gloss

type WangTileSet = TileSet Wang

data Wang = Wang
  { tColors :: M.Map Edge Color
  } deriving (Eq,Show)

instance (Applicative m, MonadRandom m) => TileLogic m Wang where
  fillTileMap ts tm = tmTraverseKeys pickTile tm
    where
    pickTile = randomWangTile ts tm
  ppTile t = ppRows
    [ [ " "     , eg North , " "     ]
    , [ eg West , " "      , eg East ]
    , [ " "     , eg South , " "     ]
    ]
    where
    eg e = case edgeColor e t of
      Red    -> "R"
      Green  -> "G"
      Yellow -> "Y"
      Blue   -> "B"

randomWangTile :: (Eq c, Ord c, Num c, MonadRandom m)
  => TileSet Wang -> TileMap c
  -> Coord c -> m TileIndex
randomWangTile ts tm cd = tsRandomIndex suitable
  where
  suitable = selectTiles cs ts
  cs = colorConstraints
         [ ( edge
           , edgeColor (oppEdge edge) $
             tsIndex ts prev
           )
         | (edge,mkPrev) <- [(West,left),(North,above)]
         , Just prev     <- [tmLookup tm $ mkPrev cd]
         ]
  left  = col -~ 1
  above = row -~ 1

-- Color / Edge {{{

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

mkTile :: Color -> Color -> Color -> Color -> Wang
mkTile n s w e = Wang $ M.fromList
  [ ( North , n )
  , ( South , s )
  , ( West  , w )
  , ( East  , e )
  ]

mkTiles :: [(Color,Color,Color,Color)] -> TileSet Wang
mkTiles = tsFromList . zip [0..] . map (uncurry4 mkTile)

tileColors :: Wang -> [(Edge,Color)]
tileColors = M.assocs . tColors

edgeColor :: Edge -> Wang -> Color
edgeColor e t = tColors t M.! e

oppEdge :: Edge -> Edge
oppEdge e = case e of
  North -> South
  South -> North
  West  -> East
  East  -> West

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

-- WangConstraint {{{

data WangConstraint
  = ConstrainEdge Edge Color
  deriving (Eq,Ord,Show)

type WangConstraints = [WangConstraint]

colorConstraints :: [(Edge,Color)] -> WangConstraints
colorConstraints = map (uncurry ConstrainEdge)

satisfies :: WangConstraints -> Wang -> Bool
satisfies cs t = foldl sat True cs
  where
  sat False _ = False
  sat _ (ConstrainEdge e c) = edgeColor e t == c

selectTiles :: WangConstraints -> WangTileSet -> WangTileSet
selectTiles cs = tsFilter $ satisfies cs

-- }}}

-- TileSets {{{

wangTiles2x2 :: TileSet Wang
wangTiles2x2 = mkTiles
  [ (n,s,w,e)
  | isSame <- [False,True]
  , n <- verticals
  , s <- if isSame then [n] else delete n verticals
  , w <- horizontals
  , e <- if isSame then [w] else delete w horizontals
  ]
  where
  verticals   = [Red,Green]
  horizontals = [Yellow,Blue]

wangTiles2x3 :: TileSet Wang
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

