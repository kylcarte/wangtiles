{-# LANGUAGE OverloadedStrings #-}

import Data.Grid
import Data.Points
import Data.Surrounding
import Data.TileMap
import Data.TileMaps
import Data.TileSet
import Random
import Render
import Render.BackEnd
import Render.BackEnd.Gloss
import Texture
import Texture.Config

import Tile
import Tile.Load
import Tile.Legend
import Tile.Neighborhood
import Tile.Wang
import Util

import Control.Monad
import System.Environment
import System.Exit

-- TODO: * support layers of texture rendering
--       * improve efficiency/memory usage for large grids

parseArgs :: IO FilePath
parseArgs = do
  as <- getArgs
  case as of
    [f] -> return f
    -- [c,r] -> return $ mkSize (read c) (read r)
    _       -> usage >> exitFailure

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  ./tiles <file>"

main :: IO ()
main = do
  mapFile <- parseArgs
  tss  <- loadStdConfigs "data/tilesets.conf"
  blob <- loadTexturesFrom tss "blob"

  td <- buildTileData mapFile
    [ ( "neighborhood"
      , withLegend $ fillTileMapByIndex
          [ ( "Wall"  , neighborhood8 )
          -- , ( "Floor" , neighborhood4 )
          -- , ( "Water" , neighborhood4 )
          ]
      )
    ]

  let glossConfig = mkDefaultGlossConfig $ baseSize td
  putStrLn $ ppTileData td
  displayBaseTileMap glossConfig td
    $ selectNeighborhoodTile td 
      [ ( "Wall" , blob ) ]

