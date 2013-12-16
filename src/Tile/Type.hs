{-# LANGUAGE TypeFamilies #-}

module Tile.Type where

import Control.Monad.Trans.Random
import Data.Grid
import Data.Points
import Data.TileMap
import Data.TileSet
import Display
import Tile
import Util

import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Graphics.Gloss.Data.Color as Gloss

instance TileLogic Wang where
  type HasTileSet Wang = HasWangTileSet
  lookupTile tss = tsLookup $ wangTileSet tss

-- Wang {{{

data Wang = Wang
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

satisfies :: WangConstraints -> Wang -> Bool
satisfies cs t = S.foldl sat True cs
  where
  sat False _ = False
  sat _ (ConstrainEdge e c) = edgeColor e t == c

selectTiles :: WangConstraints -> WangTileSet -> WangTileSet
selectTiles cs = tsFilter $ satisfies cs

-- }}}

-- WangTileSet {{{

class HasWangTileSet tss where
  wangTileSet :: tss -> WangTileSet

type WangTileSet = TileSet Wang

wangTileAt :: WangTileSet -> TileIndex -> Wang
wangTileAt ts i = wangTexture ts i

wangTexture :: WangTileSet -> TileIndex -> Wang
wangTexture = tsIndex



-- }}}

-- TileMap {{{

type WangTextureSet = TextureSet Wang

wangTileMapByIndex :: (Integral c, Ord c) => WangTextureSet
  -> TileIndex -> TileMap c -> Random (TileMap c)
wangTileMapByIndex ts ti tm = wangTileMapAt ts tm $ tmSubMapByValue ti tm

-- Wang-tile the contents of the TileMap.
wangTileMapAt :: (Integral c) => WangTextureSet -> TileMap c
  -> Coords c -> Random (TileMap c)
wangTileMapAt ts tm = csGenerateTileMapA $ randomWangTile (textureSet ts) tm

wangTileMap :: (Num c, Ord c) => WangTextureSet
  -> TileMap c -> Random (TileMap c)
wangTileMap ts tm = tmTraverseKeys (randomWangTile (textureSet ts) tm) tm

-- TODO: generalize. how do we capture which tiles have already been
--   handled?
-- * look at surrounding tiles, with potential respective constraints
-- * filter out which tiles don't need to provide constraints
-- * gather constraints and select an appropriate tile.
randomWangTile :: (Eq c, Ord c, Num c) => WangTileSet
  -> TileMap c -> Coord c -> Random TileIndex
randomWangTile ts tm cd = tsRandomIndex suitable
  where
  left  = cd & col -~ 1
  above = cd & row -~ 1
  suitable = selectTiles cs ts
  cs = colorConstraints $ concat
         [ if cd^.col == 0 then [] else [ ( West  , edgeColorAt left  East  ) ]
         , if cd^.row == 0 then [] else [ ( North , edgeColorAt above South ) ]
         ]
  edgeColorAt c e = edgeColor e . wangTileAt ts . flip tmIndex c $ tm

-- }}}

-- Pretty Printing {{{

ppWT :: Wang -> String
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

