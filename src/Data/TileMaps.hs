{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TileMaps where

import Data.TileMap

import qualified Data.Map as M
import Data.Text (Text)

newtype TileMaps c = TileMaps
  { tileMaps :: M.Map Text (TileMap c)
  } deriving (Eq,Show)

-- Building {{{

tmsFromList :: [(Text,TileMap c)] -> TileMaps c
tmsFromList = TileMaps . M.fromList

-- }}}

-- Accessing {{{

tmsLookup :: TileMaps c -> Text -> Maybe (TileMap c)
tmsLookup tms t = M.lookup t $ tileMaps tms

-- }}}

