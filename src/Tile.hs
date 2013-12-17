
module Tile where

import Data.Points
import Data.TileMap

import Data.List ((\\))

{-
-- TileLogic / HasTileSet machinery {{{

class TileLogic t where
  type HasTileSets t tss :: Constraint
  type Index t
  type Params t
  lookupTile :: HasTileSets t tss => 
    tss -> Index t -> Maybe t
  default lookupTile ::
       (Index t ~ TileIndex, HasTileSet t tss)
    => tss -> TileIndex -> Maybe t
  lookupTile tss i = tsLookup (getTileSet tss) i

instance (Index t ~ TileIndex, TileLogic t, TileLogic u)
  => TileLogic (t :.: u) where
  type HasTileSets (t :.: u) tss =
    (HasTileSet t tss,HasTileSets u tss)
  type Index (t :.: u) = Index t :.: Index u
  type Params (t :.: u) = Index t :.: Index u
  lookupTile tss (pt :.: pu) = (:.:)
    <$> ((`tsLookup` pt) $ getTileSet tss)
    <*> (lookupTile tss pu)

class HasTileSet t tss where
  getTileSet :: tss -> TileSet t

data a :.: b = a :.: b deriving (Eq,Show)
infixr 4 :.:

-- }}}
-}

-- Examples {{{

neighTM :: TileMap Int
neighTM = tmFromList $ zip
  (allCoords sz' \\
    [ mkCoord 0 3
    , mkCoord 0 4
    , mkCoord 1 4
    ])
  [0..]
  where
  sz' = mkSize 10 5

-- }}}

