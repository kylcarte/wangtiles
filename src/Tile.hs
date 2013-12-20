{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Tile where

import Data.Points
import Data.TileMap
import Data.TileSet
import Error
import Tile.Legend

import Control.Applicative
import Data.Text (Text, unpack)
import GHC.Exts (Constraint)

class (Applicative m, Monad m, Show t) => TileLogic m t | t -> m where
  type CoordConstraints t c :: Constraint
  fillTileMap :: (CoordType c, CoordConstraints t c)
    => TileSet t -> TileMap c -> SubMap c -> ErrorT m (TileMap c)
  ----
  ppTile :: t -> String
  ----

  fillTileMapByIndex :: (CoordType c, CoordConstraints t c)
    => [(Text,TileSet t)] -> Legend -> TileMap c -> ErrorT m (TileMap c)
  fillTileMapByIndex tls leg tm = do
    tms <- tsTraverseWithKey fillOne $ tmSubMapSetByIndex tm
    return $ tmUnions $ tsValues tms
    where
    err i = "fillTileMapByIndex at index " ++ show i
    fillOne i sm = wrapFail (err i) $ do
      tss <- buildTileSets leg tls
      case dropError $ tsLookup tss i of
        Nothing -> return $ emptyTileMap
        Just ts -> fillTileMap ts tm sm
    
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

