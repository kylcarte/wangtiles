{-# LANGUAGE Rank2Types #-}

module Util where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List (delete)
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Traversable as T
import System.Random (RandomGen(..))
import qualified System.Random as R

-- Random {{{

newtype Random a = Random
  { unRandom :: RandomGen g => State g a
  }

instance Functor Random where
  fmap f (Random m) = Random $ fmap f m

instance Applicative Random where
  pure = return
  (<*>) = ap

instance Monad Random where
  return a = Random $ state $ \g -> (a,g)
  (Random m) >>= f = Random $ m >>= unRandom . f

runRandom :: RandomGen g => g -> Random a -> (a,g)
runRandom g m = runState (unRandom m) g

-- Key functions

randomKey :: I.IntMap a -> Random Int
randomKey m = rnd $ first (ks !!) . R.randomR (0,n)
  where
  ks = I.keys m
  n  = length ks - 1

{-
randomKey :: (Ord k) => M.Map k a -> Random k
randomKey m = rnd $ first (ks !!) . R.randomR (0,n)
  where
  ks = M.keys m
  n = length ks - 1
-}

rnd :: (forall g. RandomGen g => g -> (a,g)) -> Random a
rnd f = Random $ state f

-- }}}

choose :: Eq a => Int -> [a] -> IO [a]
choose _ [] = return []
choose 0 _  = return []
choose n as = do
  i <- R.randomRIO (0,length as - 1)
  let a = as !! i
  as' <- choose (n-1) (delete a as)
  return $ a : as'

mkIndexMap :: [[Maybe Int]] -> I.IntMap (Int,Int)
mkIndexMap rs = I.fromList
  [ (i,(x,y))
  | (r,y)      <- zip rs [0..]
  , (Just i,x) <- zip r  [0..]
  ]

mkGridMap :: [[Maybe a]] -> M.Map (Int,Int) a
mkGridMap rs = M.fromList
  [ ((x,y),a)
  | (r,y)      <- zip rs [0..]
  , (Just a,x) <- zip r  [0..]
  ]

tzipWith :: T.Traversable f => (a -> b -> c) -> f a -> [b] -> f c
tzipWith f fa l = fc
  where
  (_,fc) = T.mapAccumL g l fa
  g [] _ = error "tzipWith: empty list"
  g (b:bs) a = (bs,f a b)

tzip :: T.Traversable f => f a -> [b] -> f (a,b)
tzip = tzipWith (,)

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile f = either fail return . eitherDecode =<< BS.readFile f

