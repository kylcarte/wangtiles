{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
  ( module Util
  , on
  ) where

import Control.Monad.Trans.Random

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Bifunctor
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.IntMap as I
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Linear
import System.Exit
import Text.Show.Pretty (ppShow)

type TileIndex = Int

-- HandleIO {{{

class (Functor m) => HandleIO m where
  io :: String -> m a -> IO a
  io' :: m a -> IO a
  io' = io "HandleIO failure"

{-
instance HandleIO Random where
  io _ = runRandomIO
  io'  = io "failure in Random"
-}

instance (Show e) => HandleIO (Either e) where
  io msg m = case m of
    Left e -> do putStrLn $ msg ++ ": " ++ show e
                 exitFailure
    Right a -> return a
  io' = either err return
    where
    err e = fail $ "failure at: " ++ show e

instance HandleIO Maybe where
  io msg = maybe (fail msg) return

-- }}}

-- Traversals {{{

tzipWith :: T.Traversable f => (a -> b -> c) -> f a -> [b] -> f c
tzipWith f fa l = fc
  where
  (_,fc) = T.mapAccumL g l fa
  g [] _ = error "tzipWith: empty list"
  g (b:bs) a = (bs,f a b)

tzip :: T.Traversable f => f a -> [b] -> f (a,b)
tzip = tzipWith (,)

-- }}}

-- JSON {{{

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile f = io errMsg . eitherDecode =<< BS.readFile f
  where
  errMsg = "couldn't parse file '" ++ f ++ "' to JSON"

-- }}}

-- Random {{{

randomKey :: I.IntMap a -> Random Int
randomKey m = (ks !!) <$> randomR (0,n)
  where
  ks = I.keys m
  n  = length ks - 1

-- }}}

-- Pretty Printing {{{

disp :: Show a => a -> IO ()
disp = putStrLn . ppShow

ppRows :: [[String]] -> String
ppRows = intercalate "\n" . map unwords

-- }}}

-- Numerical {{{

withFloat :: (Float -> Float) -> Int -> Int
withFloat f = fromEnum . f . toEnum

safeMinimum :: (Num (f a), Ord a, F.Foldable t, Additive f)
  => t (f a) -> f a
safeMinimum = F.foldl' (liftU2 min) 0

safeMaximum :: (Num (f a), Ord a, F.Foldable t, Additive f)
  => t (f a) -> f a
safeMaximum = F.foldl' (liftU2 max) 0

float :: (Real f, Enum f, Fractional f, Enum c) => Prism' f c
float = prism' toF toC
  where
  toF :: (Enum c, Fractional f) => c -> f
  toF = fromRational.toEnum.fromEnum
  toC :: (Enum c, Real f, Enum f) => f -> Maybe c
  toC r = if r == toEnum rt
    then Just $ toEnum rt
    else Nothing
    where
      rt = fromEnum $ toRational r

onFloat :: (Enum f, Fractional f, Real f, Enum c) =>
  (f -> f) -> c -> Maybe c
onFloat = underPrism float

fromFloat :: (Enum f, Fractional f, Real f, Enum c) =>
  f -> Maybe c
fromFloat = preview float

toFloat :: (Enum f, Fractional f, Real f, Enum c) =>
  c -> f
toFloat = review float

-- }}}

-- Application combinators {{{

onlyIndex :: (i -> b) -> i -> a -> b
onlyIndex = flip . const

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

(.:.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:.) = (.) . (.)

on2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
on2 o f g x = f x `o` g x

infixr .:.


if' :: a -> a -> Bool -> a
if' t f b = if b
  then t
  else f

onPair :: (a -> b) -> (a,a) -> (b,b)
onPair f (x,y) = (f x,f y)

-- }}}

-- Tuple {{{

dup :: a -> (a,a)
dup a = (a,a)

-- }}}

-- V2 {{{

toV2 :: (c -> c -> b) -> V2 c -> b
toV2 f c = f (c^._x) (c^._y)

-- }}}

-- Lenses, etc. {{{

view2 :: Getting a s a -> Getting a s a -> s -> (a,a)
view2 = on (&&&) view

lowerIso :: Iso' down up -> (up -> a) -> down -> a
lowerIso i f = f . view i

liftIso :: Iso' down up -> (down -> a) -> up -> a
liftIso i f = f . view (from i)

lift2Iso :: Iso' down up -> (down -> a -> b) -> up -> a -> b
lift2Iso i = curry . lmap (first $ down i) . uncurry

up :: Iso' down up -> down -> up
up i = view i

down :: Iso' down up -> up -> down
down i = view $ from i

upM :: (Monad m) => Iso' down up -> down -> m up
upM i = perform i

downM :: (Monad m) => Iso' down up -> up -> m down
downM i = perform $ from i

lowerIsoM :: (Monad m) => Iso' down up -> (up -> m a) -> down -> m a
lowerIsoM i f = perform $ i . act f

liftIsoM :: (Monad m) => Iso' down up -> (down -> m a) -> up -> m a
liftIsoM i f = perform $ from i . act f

lift2IsoM :: (Monad m) => Iso' down up -> (down -> a -> m b) -> up -> a -> m b
lift2IsoM i =
    perform
  . act
  . curry
  . lmap (first $ down i)
  . uncurry

underPrism :: Prism' down up -> (down -> down) -> up -> Maybe up
underPrism pr f = preview pr . f . review pr

-- }}}

-- List selection {{{

deleteGridIndices :: [(Int,Int)] -> [[a]] -> [[a]]
deleteGridIndices is rs =
  [ deleteIndices (I.lookup i im) r
  | (r,i) <- zip rs [0..]
  ]
  where
  im = groupIndices is

groupIndices :: [(Int,Int)] -> I.IntMap [Int]
groupIndices = foldr f I.empty
  where
  f (x,y) = I.alter (g y) x
  g y Nothing   = Just [y]
  g y (Just ys) = Just $ y:ys

deleteIndices :: Maybe [Int] -> [a] -> [a]
deleteIndices Nothing   as = as
deleteIndices (Just is) as =
  [ a
  | (a,i) <- zip as [0..]
  , i `notElem` is
  ]

-- }}}

