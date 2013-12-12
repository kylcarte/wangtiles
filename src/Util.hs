{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List (delete,intercalate)
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import System.Exit
import System.Random (RandomGen(..))
import qualified System.Random as R
import Text.Show.Pretty (ppShow)

type TileIndex = Int

-- Enum2 {{{

class Enum2 n where
  toEnum2   :: Enum m => n -> (m,m)
  fromEnum2 :: Enum m => (m,m) -> n
  cast :: Enum2 m => n -> m
  cast n = fromEnum2 (toEnum2 n :: (Int,Int))
  enum2 :: Enum m => m -> n
  enum2 = fromEnum2 . (id &&& id) . fromEnum

-- }}}

-- Coord {{{

data Coord = Coord
  { row :: Int
  , col :: Int
  } deriving (Eq,Ord,Show)

instance Num Coord where
  (+) = cXY (+)
  (*) = cXY (*)
  (-) = cXY (-)
  negate = onCoord negate
  abs    = onCoord abs
  signum = onCoord signum
  fromInteger = fromColRow
    . (fromInteger &&& fromInteger)

instance Enum2 Coord where
  toEnum2 = onPair toEnum . colRow
  fromEnum2 = fromColRow . onPair fromEnum

cXY :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
cXY f c1 c2 = Coord
  { col = col c1 `f` col c2
  , row = row c1 `f` row c2
  }

instance FromJSON Coord where
  parseJSON (Object o) =
        Coord
    <$> o .: "row"
    <*> o .: "col"
  parseJSON _ = mzero

onHorizontal :: (Int -> Int) -> Coord -> Coord
onHorizontal f c = c { col = f $ col c }

onVertical :: (Int -> Int) -> Coord -> Coord
onVertical f c = c { row = f $ row c }

onCoord :: (Int -> Int) -> Coord -> Coord
onCoord f c = c
  { row = f $ row c
  , col = f $ col c
  }

fromColRow :: (Int,Int) -> Coord
fromColRow (c,r) = Coord
  { col = c
  , row = r
  }

colRow :: Coord -> (Int,Int)
colRow = col &&& row

colsRows :: [Coord] -> ([Int],[Int])
colsRows = map col &&& map row

fFromCoord :: (Coord -> a) -> Int -> Int -> a
fFromCoord f c r = f $ Coord { col = c , row = r }

fToCoord :: (Int -> Int -> a) -> Coord -> a
fToCoord f c = f (col c) (row c)

cReflId, cReflX, cReflY, cProjX, cProjY :: Coord -> Coord
cReflId c = Coord { col = row c , row = col c }
cReflX = (* Coord { col =     1 , row =  (-1) })
cReflY = (* Coord { col =  (-1) , row =     1 })
cProjX c =  Coord { col = col c , row =     0 }
cProjY c =  Coord { col =     0 , row = row c }

-- }}}

-- Size {{{

data Size = Size
  { height :: Int
  , width  :: Int
  } deriving (Eq,Ord,Show)

instance Num Size where
  (+) = sXY (+)
  (*) = sXY (*)
  (-) = sXY (-)
  negate = onSize negate
  abs    = onSize abs
  signum = onSize signum
  fromInteger = fromWidthHeight
    . (fromInteger &&& fromInteger)

instance Enum2 Size where
  toEnum2 = onPair toEnum . widthHeight
  fromEnum2 = fromWidthHeight . onPair fromEnum

sXY :: (Int -> Int -> Int) -> Size -> Size -> Size
sXY f s1 s2 = Size
  { width  = width  s1 `f` width  s2
  , height = height s1 `f` height s2
  }

instance FromJSON Size where
  parseJSON (Object o) =
        Size
    <$> o .: "height"
    <*> o .: "width"
  parseJSON _ = mzero

fromWidthHeight :: (Int,Int) -> Size
fromWidthHeight (w,h) = Size
  { width  = w
  , height = h
  }

onSize :: (Int -> Int) -> Size -> Size
onSize f sz = sz
  { height = f $ height sz
  , width  = f $ width sz
  }

widthHeight :: Size -> (Int,Int)
widthHeight = width &&& height

widthsHeights :: [Size] -> ([Int],[Int])
widthsHeights = map width &&& map height

fFromSize :: (Size -> a) -> Int -> Int -> a
fFromSize f w h = f $ Size { width = w , height = h }

fToSize :: (Int -> Int -> a) -> Size -> a
fToSize f s = f (width s) (height s)

sReflId, sReflX, sReflY, sProjX, sProjY :: Size -> Size
sReflId s = Size { width = height s , height =  width s }
sReflX = (* Size { width =        1 , height =     (-1) })
sReflY = (* Size { width =     (-1) , height =        1 })
sProjX  s = Size { width =  width s , height =        0 }
sProjY  s = Size { width =        0 , height = height s }

-- }}}

-- FSize {{{

data FSize = FSize
  { fHeight :: Float
  , fWidth  :: Float
  } deriving (Eq,Ord,Show)

instance Num FSize where
  (+) = fXY (+)
  (*) = fXY (*)
  (-) = fXY (-)
  negate = onFSize negate
  abs    = onFSize abs
  signum = onFSize signum
  fromInteger = fromFWidthHeight
    . (fromInteger &&& fromInteger)

instance Fractional FSize where
  (/)   = fXY (/)
  recip = onFSize recip
  fromRational = fromFWidthHeight
    . (fromRational &&& fromRational)

instance Enum2 FSize where
  toEnum2 = onPair (toEnum . fromEnum) . fWidthHeight
  fromEnum2 = fromFWidthHeight . onPair (toEnum . fromEnum)

fXY :: (Float -> Float -> Float) -> FSize -> FSize -> FSize
fXY f f1 f2 = FSize
  { fWidth  = fWidth  f1 `f` fWidth  f2
  , fHeight = fHeight f1 `f` fHeight f2
  }

fromFWidthHeight :: (Float,Float) -> FSize
fromFWidthHeight (w,h) = FSize
  { fWidth  = w
  , fHeight = h
  }

onFSize :: (Float -> Float) -> FSize -> FSize
onFSize f sz = sz
  { fHeight = f $ fHeight sz
  , fWidth  = f $ fWidth sz
  }

fWidthHeight :: FSize -> (Float,Float)
fWidthHeight = fWidth &&& fHeight

fWidthsHeights :: [FSize] -> ([Float],[Float])
fWidthsHeights = map fWidth &&& map fHeight

fFromFSize :: (FSize -> a) -> Float -> Float -> a
fFromFSize f w h = f $ FSize { fWidth = w , fHeight = h }

fToFSize :: (Float -> Float -> a) -> FSize -> a
fToFSize f s = f (fWidth s) (fHeight s)

fReflId, fReflX, fReflY, fProjX, fProjY :: FSize -> FSize
fReflId f = FSize { fWidth = fHeight f , fHeight =  fWidth f }
fReflX = (* FSize { fWidth =         1 , fHeight =      (-1) })
fReflY = (* FSize { fWidth =      (-1) , fHeight =         1 })
fProjX  f = FSize { fWidth =  fWidth f , fHeight =         0 }
fProjY  f = FSize { fWidth =         0 , fHeight = fHeight f }

-- }}}

-- HandleIO {{{

class (Functor m) => HandleIO m where
  io :: String -> m a -> IO a
  io' :: m a -> IO a
  io' = io "HandleIO failure"

instance HandleIO Random where
  io _ = runRandomIO

wangIO :: Random a -> IO a
wangIO = io "failure in Random"

instance (Show e) => HandleIO (Either e) where
  io msg m = case m of
    Left e -> do putStrLn $ msg ++ ": " ++ show e
                 exitFailure
    Right a -> return a

neighborhoodIO :: Either Coord a -> IO a
neighborhoodIO = io "Couldn't find a suitable Neighborhood for Coord"
    
instance HandleIO Maybe where
  io msg = maybe (fail msg) return

-- }}}

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

groupIndices :: [Coord] -> I.IntMap [Int]
groupIndices = foldr (f . colRow) I.empty
  where
  f (x,y) = I.alter (g x) y
  g x Nothing   = Just [x]
  g x (Just xs) = Just $ x:xs

deleteGridIndices :: [Coord] -> [[a]] -> [[a]]
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

mkIndexMap :: [[Maybe Int]] -> I.IntMap Coord
mkIndexMap rs = I.fromList
  [ (i,Coord { col = x , row = y })
  | (r,y)      <- zip rs [0..]
  , (Just i,x) <- zip r  [0..]
  ]

mkGridMap :: [[Maybe a]] -> M.Map Coord a
mkGridMap rs = M.fromList
  [ (Coord { col = x , row = y },a)
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
decodeFile f = io errMsg . eitherDecode =<< BS.readFile f
  where
  errMsg = "couldn't parse file '" ++ f ++ "' to JSON"

onPair :: (a -> b) -> (a,a) -> (b,b)
onPair f (x,y) = (f x,f y)

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

lookupIO :: (Ord k, Show k) => k -> M.Map k a -> IO a
lookupIO k = maybe err return . M.lookup k
  where
  err = fail $ "unbound key: " ++ show k

mapUpdateAt :: (Ord k) => [k] -> (a -> a) -> M.Map k a -> M.Map k a
mapUpdateAt ks = mapUpdateWithKeyAt ks . const

mapUpdateWithKeyAt :: (Ord k) => [k] -> (k -> a -> a)
  -> M.Map k a -> M.Map k a
mapUpdateWithKeyAt ks f mp = F.foldl fn mp ks
  where
  fn m k = M.insert k (f k $ m M.! k) m

mapUpdateAtM :: (Ord k, Monad m) => [k] -> (a -> m a)
  -> M.Map k a -> m (M.Map k a)
mapUpdateAtM ks = mapUpdateWithKeyAtM ks . const

mapUpdateWithKeyAtM :: (Ord k, Monad m) => [k] -> (k -> a -> m a)
  -> M.Map k a -> m (M.Map k a)
mapUpdateWithKeyAtM ks f mp = F.foldlM fn mp ks
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

withFloat :: (Float -> Float) -> Int -> Int
withFloat f = fromEnum . f . toEnum

(.:.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:.) = (.) . (.)

infixr .:.

