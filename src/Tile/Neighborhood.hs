
module Tile.Neighborhood where

import Control.Applicative
import Control.Arrow (first,second)
import Control.Monad
import Data.List  (transpose)
import Data.Maybe (fromMaybe)
import Tile
import Util

import Graphics.Gloss

-- Neighborhoods {{{

-- TODO: decode at compile time with TH
neighborhood4 :: TileSet Neighborhood
neighborhood4 = decodeNeighborhood4s
  [ [ [ "..." , "..." , "..." , "..." ]
    , [ ".o." , ".oo" , "ooo" , "oo." ]
    , [ ".o." , ".o." , ".o." , ".o." ]
    ] ---------------------------------
  , [ [ ".o." , ".o." , ".o." , ".o." ]
    , [ ".o." , ".oo" , "ooo" , "oo." ]
    , [ ".o." , ".o." , ".o." , ".o." ]
    ] ---------------------------------
  , [ [ ".o." , ".o." , ".o." , ".o." ]
    , [ ".o." , ".oo" , "ooo" , "oo." ]
    , [ "..." , "..." , "..." , "..." ]
    ] ---------------------------------
  , [ [ "..." , "..." , "..." , "..." ]
    , [ ".o." , ".oo" , "ooo" , "oo." ]
    , [ "..." , "..." , "..." , "..." ]
    ]
  ]

neighborhood8 :: TileSet Neighborhood
neighborhood8 = decodeNeighborhood8s
  [ [ [ "..." , "..." , "..." , "..." , "..." , "..." , "..." , "ooo" , "ooo" , "ooo" ]
    , [ ".o." , ".oo" , "ooo" , "oo." , ".oo" , "ooo" , "oo." , "ooo" , "ooo" , "ooo" ]
    , [ ".o." , ".oo" , "ooo" , "oo." , ".o." , ".o." , ".o." , "oo." , ".o." , ".oo" ]
    ] ---------------------------------------------------------------------------------
  , [ [ ".o." , ".oo" , "ooo" , "oo." , ".o." , "ooo" , ".o." , "oo." , ".o." , ".oo" ]
    , [ ".o." , ".oo" , "ooo" , "oo." , ".oo" , "o.o" , "oo." , "ooo" , "ooo" , "ooo" ]
    , [ ".o." , ".oo" , "ooo" , "oo." , ".o." , "ooo" , ".o." , "oo." , ".o." , ".oo" ]
    ] ---------------------------------------------------------------------------------
  , [ [ ".o." , ".oo" , "ooo" , "oo." , ".o." , ".o." , ".o." , "oo." , ".o." , ".oo" ]
    , [ ".o." , ".oo" , "ooo" , "oo." , ".oo" , "ooo" , "oo." , "ooo" , "ooo" , "ooo" ]
    , [ "..." , "..." , "..." , "..." , "..." , "..." , "..." , "ooo" , "ooo" , "ooo" ]
    ] ---------------------------------------------------------------------------------
  , [   {---} [ "..." , "..." , "..." , "..." , ".oo" , "oo." , "..." , "oo." , ".oo" ]
    ,   {---} [ ".oo" , "ooo" , "oo." , "ooo" , ".oo" , "oo." , "ooo" , "ooo" , "ooo" ]
    ,   {---} [ "..." , "..." , "..." , "oo." , ".o." , ".o." , ".oo" , ".o." , ".o." ]
    ] ---------------------------------------------------------------------------------
  , [   {---}   {---} [ "oo." , ".oo" , ".o." , ".oo" , "oo." , ".o." , ".o." , ".o." ]
    ,   {---}   {---} [ "ooo" , "ooo" , ".oo" , "ooo" , "ooo" , "oo." , "ooo" , "ooo" ]
    ,   {---}   {---} [ ".oo" , "oo." , ".oo" , "..." , "..." , "oo." , "oo." , ".oo" ]
    ]
  ]

-- }}}

-- Neighborhood {{{

tmMatch :: TileMap -> Neighborhood -> Coord -> Bool
tmMatch = gridMatch . tileMap

data Neighborhood = Neighborhood
  { nNW :: Neighbor
  , nN  :: Neighbor
  , nNE :: Neighbor
  , nE  :: Neighbor
  , nSE :: Neighbor
  , nS  :: Neighbor
  , nSW :: Neighbor
  , nW  :: Neighbor
  } deriving (Show)

type Neighbor = Maybe Bool

instance Eq Neighborhood where
  x == y = and
    [ eq nNW x y
    , eq nN  x y
    , eq nNE x y
    , eq nE  x y
    , eq nSE x y
    , eq nS  x y
    , eq nSW x y
    , eq nW  x y
    ]
    where
    eq f a b = fromMaybe True $ (==) <$> f a <*> f b

same :: Neighbor
same = Just True

diff :: Neighbor
diff = Just False

wild :: Neighbor
wild = Nothing

-- }}}

-- NeighborhoodConstraint {{{

gridMatch :: Eq a => Grid a -> Neighborhood -> Coord -> Bool
gridMatch g n c = n == gridNeighborhood g c

gridNeighborhood :: Eq a => Grid a -> Coord -> Neighborhood
gridNeighborhood g c = Neighborhood
  { nNW = (center ==) <$> corner cNW eN eW
  , nN  = (center ==) <$> edge   eN
  , nNE = (center ==) <$> corner cNE eN eE
  , nE  = (center ==) <$> edge   eE
  , nSE = (center ==) <$> corner cSE eS eE
  , nS  = (center ==) <$> edge   eS
  , nSW = (center ==) <$> corner cSW eS eW
  , nW  = (center ==) <$> edge   eW
  }
  where
  center   = gridIndex g c
  edge m   = m `mplus` return center
  corner cr e1 e2 = cr `mplus` e1 `mplus` e2 `mplus` return center
  eN  = lu . n   $ c
  eS  = lu . s   $ c
  eW  = lu . w   $ c
  eE  = lu . e   $ c
  cNW = lu . n.w $ c
  cNE = lu . n.e $ c
  cSE = lu . s.e $ c
  cSW = lu . s.w $ c
  lu = gridLookup g
  n = second pred
  s = second succ
  w = first  pred
  e = first  succ

-- }}}

-- NeighborhoodTileSet {{{

type NeighborhoodTileSet = TileSet (Picture,Neighborhood)

neighborhoodAt :: NeighborhoodTileSet -> TileIndex -> Neighborhood
neighborhoodAt ts i = snd $ neighborhoodTexture ts i

neighborhoodTexture :: NeighborhoodTileSet -> TileIndex
  -> (Picture,Neighborhood)
neighborhoodTexture = tsIndex

-- }}}

-- Decode {{{

type EncodedNeighborhoods = [[[String]]]

decodeNeighborhood4s , decodeNeighborhood8s :: 
 EncodedNeighborhoods -> TileSet Neighborhood
decodeNeighborhood4s = mkNeighborhoods . decodeNeighborhoods True
decodeNeighborhood8s = mkNeighborhoods . decodeNeighborhoods False

decodeNeighborhoods :: Bool -> EncodedNeighborhoods -> [Neighborhood]
decodeNeighborhoods isN4 =
    map decodeN
  . concat
  . map (map concat . transpose)
  where
  decodeN [nw,n,ne,w,_,e,sw,s,se] = Neighborhood
    { nNW = decodeC nw
    , nN  = decodeE n
    , nNE = decodeC ne
    , nE  = decodeE e
    , nSE = decodeC se
    , nS  = decodeE s
    , nSW = decodeC sw
    , nW  = decodeE w
    }
  decodeN s = error $ "cannot decode String to Neighborhood: " ++ show s
  decodeE c = case c of
    '.' -> diff
    'o' -> same
    '*' -> wild
    _   -> err c
  decodeC c = if isN4 then wild else case c of
    '.' -> diff
    'o' -> same
    '*' -> wild
    _   -> err c
  err c = error $ "cannot decode Char to Neighborhood: " ++ show c

mkNeighborhoods :: [Neighborhood] -> TileSet Neighborhood
mkNeighborhoods = tsFromList . zip [0..]

-- }}}

-- TODO: TileMap {{{

-- }}}

-- Pretty Printing {{{

ppNP :: Neighborhood -> String
ppNP n = ppRows
  [ [ shw $ nNW n , shw $ nN  n , shw $ nNE n ]
  , [ shw $ nW  n ,     "."     , shw $ nE  n ]
  , [ shw $ nSW n , shw $ nS  n , shw $ nSE n ]
  ]
  where
  shw Nothing  = "*"
  shw (Just p) = show p

-- }}}

