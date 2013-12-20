{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Random where

import Data.Points
import Data.Grid
import Data.TileMap
import Data.TileMaps
import Data.TileSet
import Util

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List (delete)
import Data.Text (Text)
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified System.Random as R

newtype Random a = Random
  { unRandom :: R.RandomGen g => State g a
  }

instance Functor Random where
  fmap f (Random m) = Random $ fmap f m

instance Applicative Random where
  pure = return
  (<*>) = ap

instance Monad Random where
  return a = Random $ state $ \g -> (a,g)
  (Random m) >>= f = Random $ m >>= unRandom . f

runRandom :: R.RandomGen g => Random a -> g -> (a,g)
runRandom = runState . unRandom

runRandomIO :: Random a -> IO a
runRandomIO m = do
  g <- R.getStdGen
  let (a,g') = runRandom m g
  R.setStdGen g'
  return a

-- Helpers

rnd :: (forall g. R.RandomGen g => g -> (a,g)) -> Random a
rnd f = Random $ state f

random :: R.Random a => Random a
random = rnd R.random

randomR :: R.Random a => (a,a) -> Random a
randomR rng = rnd $ R.randomR rng

choose :: Eq a => Int -> [a] -> Random [a]
choose 0 _  = return []
choose n as = do
  (ma,as') <- choose1 as
  case ma of
    Nothing -> return []
    Just a  -> (a:) <$> choose (n-1) as'

{-
choose _ [] = return []
choose 0 _  = return []
choose n as = do
  i <- randomR (0,length as - 1)
  let a = as !! i
  as' <- choose (n-1) (delete a as)
  return $ a : as'
-}

choose1 :: Eq a => [a] -> Random (Maybe a,[a])
choose1 [] = return (Nothing,[])
choose1 as = do
  i <- randomR (0, length as - 1)
  let a = as !! i
  let as' = delete a as
  return (Just a,as')

class HasKey m k | m -> k where
  getKeys   :: m -> [k]
  randomKey :: Eq k => m -> Random (Maybe k)
  randomKey m = fst <$> choose1 (getKeys m)

instance HasKey (I.IntMap a) Int where
  getKeys = I.keys

instance HasKey (TileSet a) Int where
  getKeys = getKeys . tileSet

instance HasKey (M.Map k a) k where
  getKeys = M.keys

instance HasKey (Grid c a) (Coord c) where
  getKeys = getKeys . grid

instance HasKey (TileMap c) (Coord c) where
  getKeys = getKeys . tileMap

instance HasKey (TileMaps c) Text where
  getKeys = getKeys . tileMaps

mkRandomGrid :: forall a c. (DiscreteCoord c, R.Random a)
  => (a,a) -> Size c -> Random (Grid c a)
mkRandomGrid rng sz = do
  a <- random
  T.mapM f $ mkRepeatGrid sz a
  where
  f :: a -> Random a
  f = const $ randomR rng

randomTileMap :: (DiscreteCoord c) => (TileIndex,TileIndex)
  -> Size c -> Random (TileMap c)
randomTileMap rng sz = fmap TileMap . mkRandomGrid rng $ sz

