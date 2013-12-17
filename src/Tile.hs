{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}

module Tile where

import Data.Points
import Data.TileMap
import Data.TileSet
import Util

import Control.Applicative
import Data.List ((\\))
import GHC.Exts (Constraint)

-- TileLogic / HasTileSet machinery {{{

class TileLogic t where
  type HasTileSets t tss :: Constraint
  type Params t
  lookupTile :: HasTileSets t tss => 
    tss -> Params t -> Maybe t
  default lookupTile ::
       (Params t ~ TileIndex, HasTileSet t tss)
    => tss -> TileIndex -> Maybe t
  lookupTile tss i = tsLookup (getTileSet tss) i

instance (Params t ~ TileIndex, TileLogic t, TileLogic u)
  => TileLogic (t :.: u) where
  type HasTileSets (t :.: u) tss =
    (HasTileSet t tss,HasTileSets u tss)
  type Params (t :.: u) = Params t :.: Params u
  lookupTile tss (pt :.: pu) = (:.:)
    <$> ((`tsLookup` pt) $ getTileSet tss)
    <*> (lookupTile tss pu)

class HasTileSet t tss where
  getTileSet :: tss -> TileSet t

data a :.: b = a :.: b deriving (Eq,Show)
infixr 4 :.:

-- }}}

-- Examples {{{

neighTM :: TileMap Int
neighTM = tmFromList $ zip
  (enumCoords sz' [0..lexMaxBound sz'] \\
    [ mkCoord 0 3
    , mkCoord 0 4
    , mkCoord 1 4
    ])
  [0..]
  where
  sz' = mkSize 10 5

-- }}}

