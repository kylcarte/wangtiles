{-# LANGUAGE OverloadedStrings #-}

module Data.TileMaps where

import Data.Points
import Data.TileMap
import Error

import Control.Applicative
import qualified Data.Map as M
import Data.Text (Text,unpack)

newtype TileMaps c = TileMaps
  { tileMaps :: M.Map Text (TileMap c)
  } deriving (Eq,Show)

-- Building {{{

tmsFromList :: [(Text,TileMap c)] -> TileMaps c
tmsFromList = TileMaps . M.fromList

-- }}}

-- Accessing {{{

tmsLookup :: (CoordType c) => TileMaps c -> Text -> Error (TileMap c)
tmsLookup tms t = reportNothing err $ M.lookup t $ tileMaps tms
  where
  err = "Couldn't find TileMap '" ++ unpack t ++ "' in in TileMaps:" ++
    ppTileMaps tms

tmsContents :: (CoordType c) => TileMaps c -> [(Text,TileMap c)]
tmsContents = M.assocs . tileMaps

-- }}}

-- Maps / Traversals {{{

tmsOnMapA :: (Applicative f) => (M.Map Text (TileMap c) -> f (M.Map Text (TileMap d)))
  -> TileMaps c -> f (TileMaps d)
tmsOnMapA f = fmap TileMaps . f . tileMaps

tmsTraverseWithKey :: (Applicative f) => (Text -> TileMap c -> f (TileMap d))
  -> TileMaps c -> f (TileMaps d)
tmsTraverseWithKey = tmsOnMapA . M.traverseWithKey

-- }}}

-- Pretty Printing {{{

ppTileMaps :: (CoordType c) => TileMaps c -> String
ppTileMaps = unlines . map ppOne . tmsContents
  where
  ppOne (n,tm) = unlines
    [ unpack n ++ ":"
    , ppTileMap tm
    ]

-- }}} 

