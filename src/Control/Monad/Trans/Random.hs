{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Trans.Random where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List (delete)
import qualified System.Random as R
import System.Random (RandomGen(..))

newtype RandomT m a = RandomT
  { unRandomT :: RandomGen g => StateT g m a
  }

instance (Functor m) => Functor (RandomT m) where
  fmap f (RandomT m) = RandomT $ fmap f m

instance (Functor m, Monad m) => Applicative (RandomT m) where
  pure = return
  (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (RandomT m) where
  empty = mzero
  (<|>) = mplus

instance (MonadPlus m) => MonadPlus (RandomT m) where
  mzero = RandomT mzero
  m `mplus` n = RandomT $ unRandomT m `mplus` unRandomT n

instance (Monad m) => Monad (RandomT m) where
  return a = RandomT $ state $ \g -> (a,g)
  (RandomT m) >>= f = RandomT $ m >>= unRandomT . f

instance MonadTrans RandomT where
    lift m = RandomT $ lift m

type Random = RandomT Identity

runRandom :: RandomGen g => Random a -> g -> (a,g)
runRandom m g = runIdentity $ runRandomT m g

runRandomT :: RandomGen g => RandomT m a -> g -> m (a,g)
runRandomT m g = runStateT (unRandomT m) g

-- Helpers

rnd :: (Monad m) => (forall g. RandomGen g => g -> (a,g)) -> RandomT m a
rnd f = RandomT $ state f

random :: (Monad m) => R.Random a => RandomT m a
random = rnd $ R.random

randomR :: (Monad m) => R.Random a => (a,a) -> RandomT m a
randomR rng = rnd $ R.randomR rng

choose :: (Monad m, Eq a) => Int -> [a] -> RandomT m [a]
choose _ [] = return []
choose 0 _  = return []
choose n as = do
  i <- randomR (0,length as - 1)
  let a = as !! i
  as' <- choose (n-1) (delete a as)
  return $ a : as'

