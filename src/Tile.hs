{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tile where

import Data.Points
import Data.TileMap

import Data.List ((\\))

neighTM :: TileMap Int
neighTM = tmFromList $ zip
  (enumCoords sz' [0..lexMaxBound sz'] \\
    [ mkCoord 0 3
    , mkCoord 0 4
    , mkCoord 1 4
    ])
  [0..]
  where
  sz' = mkSize 10 5

