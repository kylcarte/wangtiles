
module Data.Surrounding where

import Data.Grid
import Data.Points
import Util

import Control.Lens
import Data.Maybe (fromMaybe)

data Surrounding a = Surrounding
  { sC  :: a
  , sNW :: Maybe a
  , sN  :: Maybe a
  , sNE :: Maybe a
  , sE  :: Maybe a
  , sSE :: Maybe a
  , sS  :: Maybe a
  , sSW :: Maybe a
  , sW  :: Maybe a
  } deriving (Eq,Show)

-- Building {{{

gridSurrounding :: (Integral c) => Grid c a -> Coord c -> Surrounding a
gridSurrounding g c = Surrounding
  { sC  = gridIndex g c
  , sNW = lu . n.w $ c
  , sN  = lu . n   $ c
  , sNE = lu . n.e $ c
  , sE  = lu . e   $ c
  , sSE = lu . s.e $ c
  , sS  = lu . s   $ c
  , sSW = lu . s.w $ c
  , sW  = lu . w   $ c
  }
  where
  lu = gridLookup g
  n = row -~ 1
  s = row +~ 1
  w = col -~ 1
  e = col +~ 1

-- }}}

-- Pretty Printing {{{

ppSurrounding :: (Show a) => Surrounding a -> String
ppSurrounding sd = ppRows $ map (map pad) ss
  where
  ss = map (map $ fromMaybe "*" . fmap show . ($ sd))
    [ [ sNW , sN , sNE ]
    , [ sW  ,  c , sE  ]
    , [ sSW , sS , sSE ]
    ]
  c   = Just . sC
  pad s = replicate (mxdigits - length s) ' ' ++ s
  mxdigits = length mx
  mx  = maximum $ map maximum ss

printSurrounding :: (Show a) => Surrounding a -> IO ()
printSurrounding = putStrLn . ppSurrounding

-- }}}

