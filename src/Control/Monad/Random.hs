{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Random
  ( MonadRandom(..)
  , runRandom
  , runRandomT
  , RandomT(..)
  , Random
  ) where

import qualified Control.Monad.Trans.Random as RandomT
import Control.Monad.Trans.Random (runRandom,runRandomT,RandomT(..),Random)

import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import Control.Monad.Trans.Class (lift)
import Data.List (delete)
import Data.Monoid
import qualified System.Random as R

class Monad m => MonadRandom m where
  random  :: R.Random a => m a
  randomR :: R.Random a => (a,a) -> m a
  rnd     :: (forall g. R.RandomGen g => g -> (a,g)) -> m a
  choose  :: Eq a => Int -> [a] -> m [a]
  choose _ [] = return []
  choose 0 _  = return []
  choose n as = do
    i <- randomR (0,length as - 1)
    let a = as !! i
    as' <- choose (n-1) (delete a as)
    return $ a:as'

instance Monad m => MonadRandom (RandomT.RandomT m) where
  random  = RandomT.random
  randomR = RandomT.randomR
  rnd     = RandomT.rnd

instance MonadRandom m => MonadRandom (ContT r m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance (Error e, MonadRandom m) => MonadRandom (ErrorT e m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance MonadRandom m => MonadRandom (IdentityT m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance MonadRandom m => MonadRandom (ListT m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance MonadRandom m => MonadRandom (MaybeT m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance MonadRandom m => MonadRandom (ReaderT r m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance (Monoid w, MonadRandom m) => MonadRandom (Lazy.RWST r w s m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance (Monoid w, MonadRandom m) => MonadRandom (Strict.RWST r w s m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance (MonadRandom m) => MonadRandom (Lazy.StateT s m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance (MonadRandom m) => MonadRandom (Strict.StateT w m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance (Monoid w, MonadRandom m) => MonadRandom (Lazy.WriterT w m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

instance (Monoid w, MonadRandom m) => MonadRandom (Strict.WriterT w m) where
  random  = lift random
  randomR = lift . randomR
  rnd     = lift . rnd

