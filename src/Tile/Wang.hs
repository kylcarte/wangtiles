{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tile.Wang
  ( Wang (..)
  , Edge (..)
  , Color (..)
  , wangTiles2x2
  , wangTiles2x3
  , WangTileSet
  , WangTextureSet
  , wangTileMap
  , wangTileMapByIndex
  , mkWangPicture
  , tileColors
  , edgeAxis
  , glossColor
  , ppWT
  ) where

import Control.Monad.Trans.Random
import Data.Grid
import Data.Points
import Data.TileMap
import Data.TileSet
import Display
import Render
import Tile
import Util

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Graphics.Gloss.Data.Color as Gloss

instance TileLogic Wang where
  type HasTileSets Wang tss = HasTileSet Wang tss
  type Params Wang = TileIndex

type WangTileSet = TileSet Wang

-- Render {{{

instance (MonadReader Textures m) => RenderTile m Wang where
  mkPictureProxy _ i = do
    ts <- ask
    return $ tsIndex ts i

wangProxy :: Proxy Wang
wangProxy = Proxy

mkWangPicture :: (MonadReader Textures m) => TileIndex -> m Picture
mkWangPicture = mkPictureProxy wangProxy

-- }}}

-- Tiles {{{

wangTiles2x2 :: TileSet Wang
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

-- TileMap {{{

type WangTextureSet = TextureSet Wang

wangTileMapByIndex :: (Integral c, Ord c) => WangTextureSet
  -> TileIndex -> TileMap c -> Random (TileMap c)
wangTileMapByIndex ts ti tm = wangTileMapAt ts tm $ tmSubMapByValue ti tm

-- Wang-tile the contents of the TileMap.
wangTileMapAt :: (Integral c) => WangTextureSet -> TileMap c
  -> Coords c -> Random (TileMap c)
wangTileMapAt ts tm = csGenerateTileMapA $ randomWangTile (snd <$> textureSet ts) tm

wangTileMap :: (Num c, Ord c) => WangTextureSet
  -> TileMap c -> Random (TileMap c)
wangTileMap ts tm = tmTraverseKeys (randomWangTile (snd <$> textureSet ts) tm) tm

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
  edgeColorAt c e = edgeColor e . tsIndex ts . flip tmIndex c $ tm

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

