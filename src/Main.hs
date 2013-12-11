{-# LANGUAGE OverloadedStrings #-}

import Config.Render
import Config.Render.Wang
import Config.TileSet
import Config.TileSet.Neighborhood
import Config.TileSet.Wang
import Display
import Display.Neighborhood
import Display.Wang
import Texture
import Tile
import Tile.Neighborhood
import Tile.Wang
import Util

import Data.List ((\\))
import qualified Data.Text as T
import System.Environment
import System.Exit
import Text.Show.Pretty

-- TODO: * support layers of texture rendering
--       * improve efficiency/memory usage for large grids

parseArgs :: IO (Int,Int)
parseArgs = do
  as <- getArgs
  case as of
    [c,r] -> return (read c, read r)
    _       -> usage >> exitFailure

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn $ "  ./wangtiles <# cols> <# rows>"

main :: IO ()
main = do
  sz <- parseArgs
  rTM <- runRandomIO $ randomTileMap (0,1) sz
  -- putStrLn $ ppTileMap rTM

  tsc <- loadTileSet "data/tilesets.conf" "blob"
  ts  <- loadNeighborhoodTileSet tsc neighborhood8

  let rc = defaultRenderConfig
  let nts = mkNeighborhoodTextureSet tsc ts
  let tms = tmSubMap 0 rTM
  -- putStrLn $ ppTileMap tms
  tm <- either badCoord return $ neighborhoodTileMap ts 0 rTM
  displayTileMap rc nts sz tm
  where
  badCoord c = fail $ "Couldn't find a neighborhood for " ++ show c

