{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TileMaps where

import Data.TileMap

import Control.Lens
import qualified Data.Map as M
import Data.Text (Text)

newtype TileMaps c = TileMaps
  { _tileMaps :: M.Map Text (TileMap c)
  } deriving (Eq,Show)

makeLenses ''TileMaps

tmsLookup :: TileMaps c -> Text -> Maybe (TileMap c)
tmsLookup tms t = M.lookup t $ _tileMaps tms

