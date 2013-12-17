{-# LANGUAGE OverloadedStrings #-}

import Config.Render
import Config.Render.Wang
import Config.TileSet
import Config.TileSet.Neighborhood
import Config.TileSet.Wang
import Data.Grid
import Data.Points
import Data.Surrounding
import Data.TileMap
import Display
import Display.Neighborhood
import Display.Wang
import Texture
import Tile
import Tile.Legend
import Tile.Neighborhood
import Tile.Wang
import Util

import Control.Monad
import Data.List ((\\))
import qualified Data.Text as T
import System.Environment
import System.Exit
import Text.Show.Pretty

import Graphics.Gloss

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
  putStrLn $ "  ./wangtiles <file>"

main :: IO ()
main = do
  mapFile <- parseArgs
  tss <- loadTileSets "data/tilesets.conf"
  blob  <- loadNeighborhoodTextureSet tss "blob-grey" neighborhood8

  (tm,ts) <- readRoom mapFile
  printTileMap (tm :: TileMap Int)
  ntm <- io' $ neighborhoodTileMapByIndex blob 0 tm
  printTileMap ntm
  displayTileMap rc blob (tmSize tm) ntm
  -- displayTileMap rc blob (tmSize neighTM) neighTM

  ------------
  -- blob  <- loadNeighborhoodTextureSet tss "blob"  neighborhood8
  -- fence <- loadNeighborhoodTextureSet tss "fence" neighborhood4
  -- rtm <- io' $ randomTileMap (0,1) sz
  -- tm1 <- io' $ neighborhoodTileMapByIndex blob  0 rtm
  -- tm2 <- io' $ neighborhoodTileMapByIndex fence 1 rtm
  -- let sz' = mkSize 10 5
  -- putStrLn $ ppTileMap neighTM
  -- displayTileMap rc blob sz' neighTM
  ------------
  -- displayLayers rc sz (textureSize blob)
  --   $ map (uncurry $ renderTileMap rc) [(blob,tm1),(fence,tm2)]

  -- grass <- loadWangTextureSet wrc tss "grass" wangTiles2x2
  -- let tm = mkRepeatTileMap sz
  -- tm3 <- io' $ wangTileMap grass tm
  -- displayTileMap rc grass tm3
  where
  rc = defaultRenderConfig { windowBackground = black }
  wrc = defaultWangRenderConfig { wRenderConfig = rc }

