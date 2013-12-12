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

import Graphics.Gloss

-- TODO: * support layers of texture rendering
--       * improve efficiency/memory usage for large grids

parseArgs :: IO Size
parseArgs = do
  as <- getArgs
  case as of
    [c,r] -> return $ fromWidthHeight (read c,read r)
    _       -> usage >> exitFailure

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn $ "  ./wangtiles <# cols> <# rows>"

main :: IO ()
main = do
  sz <- parseArgs
  tss <- loadTileSets "data/tilesets.conf"

  blob  <- loadNeighborhoodTextureSet tss "blob"  neighborhood8
  fence <- loadNeighborhoodTextureSet tss "fence" neighborhood4
  rtm <- runRandomIO $ randomTileMap (0,1) sz

  tm1 <- neighborhoodIO $ neighborhoodTileMapByIndex blob  0 rtm
  tm2 <- neighborhoodIO $ neighborhoodTileMapByIndex fence 1 rtm
  displayLayers rc (tmSize rtm) (textureSize blob)
    $ map (uncurry $ renderTileMap rc) [(blob,tm1),(fence,tm2)]
  where
  rc = defaultRenderConfig { windowBackground = black }

