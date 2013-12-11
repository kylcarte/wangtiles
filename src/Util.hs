{-# LANGUAGE RankNTypes #-}

module Util where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List (delete,intercalate)
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import System.Random (RandomGen(..))
import qualified System.Random as R
import Text.Show.Pretty (ppShow)

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

runRandomIO :: Random a -> IO a
runRandomIO m = do
  g <- R.getStdGen
  let (a,g') = runRandom g m
  R.setStdGen g'
  return a

runRandom :: RandomGen g => g -> Random a -> (a,g)
runRandom g m = runState (unRandom m) g

-- Key functions

randomKey :: I.IntMap a -> Random Int
randomKey m = (ks !!) <$> randomR (0,n)
  where
  ks = I.keys m
  n  = length ks - 1

rnd :: (forall g. RandomGen g => g -> (a,g)) -> Random a
rnd f = Random $ state f

random :: R.Random a => Random a
random = rnd $ R.random

randomR :: R.Random a => (a,a) -> Random a
randomR rng = rnd $ R.randomR rng

-- }}}

choose :: Eq a => Int -> [a] -> IO [a]
choose _ [] = return []
choose 0 _  = return []
choose n as = do
  i <- R.randomRIO (0,length as - 1)
  let a = as !! i
  as' <- choose (n-1) (delete a as)
  return $ a : as'

groupIndices :: [(Int,Int)] -> I.IntMap [Int]
groupIndices = foldr f I.empty
  where
  f (x,y) = I.alter (g y) x
  g y Nothing   = Just [y]
  g y (Just ys) = Just $ y:ys

deleteGridIndices :: [(Int,Int)] -> [[a]] -> [[a]]
deleteGridIndices is rs =
  [ deleteIndices (I.lookup i im) r
  | (r,i) <- zip rs [0..]
  ]
  where
  im = groupIndices is

deleteIndices :: Maybe [Int] -> [a] -> [a]
deleteIndices Nothing   as = as
deleteIndices (Just is) as =
  [ a
  | (a,i) <- zip as [0..]
  , i `notElem` is
  ]

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

onPair :: (a -> b) -> (a,a) -> (b,b)
onPair f (x,y) = (f x,f y)

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

lookupIO :: (Ord k, Show k) => k -> M.Map k a -> IO a
lookupIO k = maybe err return . M.lookup k
  where
  err = fail $ "unbound key: " ++ show k

mapUpdateByM :: (Ord k, Monad m) => [k] -> (a -> m a)
  -> M.Map k a -> m (M.Map k a)
mapUpdateByM ks = mapUpdateWithKeyByM ks . const

mapUpdateWithKeyByM :: (Ord k, Monad m) => [k] -> (k -> a -> m a)
  -> M.Map k a -> m (M.Map k a)
mapUpdateWithKeyByM ks f mp = F.foldlM fn mp ks
  where
  fn m k = do
    a <- maybe err return $ M.lookup k m
    b <- f k a
    return $ M.insert k b m
  err = fail "key not in map"

disp :: Show a => a -> IO ()
disp = putStrLn . ppShow

ppRows :: [[String]] -> String
ppRows = intercalate "\n" . map unwords

