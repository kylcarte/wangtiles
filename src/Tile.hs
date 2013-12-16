{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tile where

import Data.Points
import Data.TileMap
import Data.TileSet
import Util

import Control.Applicative
import Data.List ((\\))
import GHC.Exts (Constraint)

data a :.: b = a :.: b deriving (Eq,Show)

infixr 4 :.:

traverseBoth :: (Applicative f) =>
  (a -> f c) -> (b -> f d) -> a :.: b -> f (c :.: d)
traverseBoth f g (a :.: b) = (:.:) <$> f a <*> g b



class TileLogic t where
  type HasTileSets t tss :: Constraint
  type Params t
  lookupTile :: HasTileSets t tss => 
    Params t -> tss -> Maybe t

instance (Params t ~ TileIndex, TileLogic t, TileLogic u) => TileLogic (t :.: u) where
  type HasTileSets (t :.: u) tss = (HasTileSet t tss,HasTileSets u tss)
  type Params (t :.: u) = Params t :.: Params u
  lookupTile (pt :.: pu) tss = (:.:)
    <$> ((`tsLookup` pt) $ getTileSet tss)
    <*> (lookupTile pu tss)

class HasTileSet t tss where
  getTileSet :: tss -> TileSet t



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

