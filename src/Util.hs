{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( module Util
  , on
  ) where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Bifunctor
import Data.Function (on)
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Linear
import Text.Show.Pretty (ppShow)

type TileIndex = Int

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

-- Pretty Printing {{{

addLine :: String -> String -> String
addLine l1 l2 = l1 ++ "\n" ++ l2

disp :: Show a => a -> IO ()
disp = putStrLn . ppShow

ppLines :: [String] -> String
ppLines = unlines

ppStringRows :: [[String]] -> String
ppStringRows = ppLines . map unwords

ppPadStringRows :: [[String]] -> String
ppPadStringRows rs = ppStringRows $ map (map pad) rs
  where
  pad s = replicate (mxdigits - length s) ' ' ++ s
  mxdigits = safeMaximum 0 $ map length $ concat rs

ppRows :: (Ord a, Show a) => [[a]] -> String
ppRows = ppPadStringRows . map (map show)

ppSparseRows :: (Ord a, Show a) => [[Maybe a]] -> String
ppSparseRows = ppPadStringRows . map (map $ maybe "" show)

ppRowsByCol :: (Ord a, Show a) => [[a]] -> String
ppRowsByCol = ppPadStringRowsByCol . map (map show)

ppSparseRowsByCol :: (Ord a, Show a) => [[Maybe a]] -> String
ppSparseRowsByCol = ppPadStringRowsByCol . map (map $ maybe "" show)

ppPadStringRows2Col :: (Ord a, Show a,Show b) => [(a,b)] -> String
ppPadStringRows2Col es = ppStringRows $ padStringsByCol (zip ls [True,False,False]) rs
  where
  rs  = map entry es
  ls = maxColumnWidths rs
  entry (a,b) = [show a,":",show b]

ppPadStringRowsByCol :: [[String]] -> String
ppPadStringRowsByCol rs = ppStringRows $ padStringsByCol (zip (maxColumnWidths rs) (repeat True)) rs

maxColumnWidths :: [[[a]]] -> [Int]
maxColumnWidths = safeMaximumV2 [] . map (map length)

padStringsByCol :: [(Int,Bool)] -> [[String]] -> [[String]]
padStringsByCol ls = map (zipWith (curry pad) ls)
  where
  pad ((n,justifyRight),s) = ($ replicate (n - length s) ' ') $ if justifyRight then (++ s) else (s ++)

-- }}}

-- Ord {{{

safeMinimumV2 :: (Ord a, F.Foldable t, Additive f)
  => f a -> t (f a) -> f a
safeMinimumV2 = F.foldl' $ liftU2 min

safeMaximumV2 :: (Ord a, F.Foldable t, Additive f)
  => f a -> t (f a) -> f a
safeMaximumV2 = F.foldl' $ liftU2 max

safeMinimum :: (Ord a) => a -> [a] -> a
safeMinimum = F.foldl' min

safeMaximum :: (Ord a) => a -> [a] -> a
safeMaximum = F.foldl' max

-- }}}

-- Numerical {{{

withFloat :: (Float -> Float) -> Int -> Int
withFloat f = fromEnum . f . toEnum

float :: (Real f, Enum f, Fractional f, Enum c) => Prism' f c
float = prism' toF toC
  where
  toF :: (Enum c, Fractional f) => c -> f
  toF = fromRational.enums
  toC :: (Enum c, Real f, Enum f) => f -> Maybe c
  toC r = if r == toEnum rt
    then Just $ toEnum rt
    else Nothing
    where
      rt = fromEnum $ toRational r

onFloat :: (Enum f, Fractional f, Real f, Enum c)
  => (f -> f) -> c -> Maybe c
onFloat = underPrism float

fromFloat :: (Enum f, Fractional f, Real f, Enum c)
  => f -> Maybe c
fromFloat = preview float

toFloat :: (Enum f, Fractional f, Real f, Enum c)
  => c -> f
toFloat = review float

enums :: (Enum a, Enum b) => a -> b
enums = toEnum . fromEnum

-- }}}

-- Application combinators {{{

onlyIndex :: (i -> b) -> i -> a -> b
onlyIndex = flip . const

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

on4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
on4 f (a,b,c,d) = (f a,f b,f c,f d)

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

swap2 :: (a,b) -> (b,a)
swap2 (a,b) = (b,a)

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

-- Lists {{{

deleteGridIndices :: [(Int,Int)] -> [[a]] -> [[a]]
deleteGridIndices is rs =
  [ deleteIndices (I.lookup i im) r
  | (r,i) <- zip rs [0..]
  ]
  where
  im = groupIndices $ map swap2 is

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

-- Proxy {{{

data Proxy a = Proxy

-- }}}

-- Maps {{{

flipMap :: (Ord k, Ord a) => M.Map k a -> M.Map a k
flipMap = M.fromList . map swap2 . M.assocs

-- }}}

