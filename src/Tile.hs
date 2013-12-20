{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Tile where

import Data.Grid
import Data.Points
import Data.TileMap
import Data.TileSet
import Error
import Tile.Legend
import Util
import Util.HandleIO

import Control.Applicative
import Data.Text (Text, unpack)
import qualified Data.Traversable as T

class (Applicative m, Monad m, Show t) => TileLogic m t | t -> m where
  fillTile :: (CoordType c) => TileSet t -> TileMap c
    -> Coord c -> ErrorT m TileIndex
  ppTile :: t -> String

fillSubMap :: (TileLogic m t, CoordType c)
  => (Coord c -> String) -> TileSet t -> TileMap c
  -> SubMap c -> ErrorT m (TileMap c)
fillSubMap fl ts tm sm = tmTraverseKeys fn $ subMap sm
  where
  fn c = wrapFail (fl c) $ fillTile ts tm c

ppFilledTileMap :: (HandleIO n, TileLogic m t, CoordType c)
  => TileSet t -> TileMap c -> ErrorT n String
ppFilledTileMap ts tm = do
  g <- T.mapM (tsLookup ts) $ tileMap tm
  let pg = fmap ppTile g
  return $ ppSparseStringRows $ gridList pg

fillSubMapsByIndex :: (TileLogic m t, CoordType c)
  => (TileIndex -> Coord c -> String) -> [(Text,TileSet t)]
  -> Legend -> TileMap c -> ErrorT m (TileMap c)
fillSubMapsByIndex fl tls leg tm = do
  tms <- tsTraverseWithKey fillOne $ tmSubMapSetByIndex tm
  return $ tmUnions $ tsValues tms
  where
  fillOne i sm = do
    tss <- buildTileSets leg tls
    case dropError $ tsLookup tss i of
      Nothing -> return $ emptyTileMap
      Just ts -> fillSubMap (fl i) ts tm sm
    
type TileSets t = TileSet (TileSet t)

buildTileSets :: (TileLogic m t) => Legend -> [(Text,TileSet t)] -> ErrorT m (TileSets t)
buildTileSets leg = fmap tsFromList . mapM resolve
  where
  resolve (n,ts) = handleError (legendIndex leg n) (fails err) (return . (,ts))
    where
    err = "Couldn't find '" ++ unpack n ++ "' in the Legend."

-- Examples {{{

neigh8TM :: TileMap Int
neigh8TM = mkIotaTileMapExcept (mkSize 10 5) [(0,3),(0,4),(1,4)]

-- }}}

