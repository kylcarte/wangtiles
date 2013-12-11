{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Tile.Wang where

import Control.Arrow (first,second)
import Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.Map as M

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

edgeColor :: Edge -> Tile -> Color
edgeColor e t = tColors t M.! e

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

type WangConstraints = S.Set WangConstraint

colorConstraints :: [(Edge,Color)] -> WangConstraints
colorConstraints = S.fromList . map (uncurry ConstrainEdge)

satisfies :: WangConstraints -> Tile -> Bool
satisfies cs t = S.foldl sat True cs
  where
  sat False _ = False
  sat _ (ConstrainEdge e c) = edgeColor e t == c

selectTiles :: WangConstraints -> WangTileSet -> WangTileSet
selectTiles cs ts = tsFromList suitables
  where
  allTiles  = tsAssocs ts
  suitables = filter sat allTiles
  sat (_,(_,t)) = satisfies cs t

-- }}}

-- WangTileSet {{{

type WangTileSet = TileSet (Picture,Tile)

wangTileAt :: WangTileSet -> TileIndex -> Tile
wangTileAt ts i = snd $ tileTexture ts i

tileTexture :: WangTileSet -> TileIndex -> (Picture,Tile)
tileTexture = tsIndex

-- }}}

-- TileMap {{{

-- Given a TileMap and a TileIndex which indicates Wang tiling,
--   generate a Wang-tiled subset of the TileMap which contains
--   only the coordinates which mapped to the given TileIndex
--   in the original TileMap.
wangSubMap :: WangTileSet -> TileIndex -> TileMap -> Random TileMap
wangSubMap ts ti = wangTileMap ts . tmSubMap ti

wangTileMap :: WangTileSet -> TileMap -> Random TileMap
wangTileMap ts tm = tmUpdateWithKeyByM coords fn tm
  where
  fn c _ = randomWangTile ts tm c
  coords = sortBy compareCoordsLexi $ tmCoords tm

randomWangTile :: WangTileSet -> TileMap -> Coord -> Random TileIndex
randomWangTile ts tm cd = tsRandomIndex suitable
  where
  left = first  pred cd
  up   = second pred cd
  suitable = selectTiles cs ts
  cs = colorConstraints $ concat
         [ if col cd == 0 then [] else [ ( West  , edgeColorAt left East  ) ]
         , if row cd == 0 then [] else [ ( North , edgeColorAt up   South ) ]
         ]
  edgeColorAt :: Coord -> Edge -> Color
  edgeColorAt c e = edgeColor e . wangTileAt ts . flip tmIndex c $ tm

-- }}}

-- Pretty Printing {{{

ppWT :: Tile -> String
ppWT t = ppRows
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

-- }}}

