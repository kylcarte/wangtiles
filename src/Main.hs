{-# LANGUAGE OverloadedStrings #-}

import Config.Render.Wang
import Config.TileSet
import Config.TileSet.Wang
import Display.Wang
import Texture
import Tile
import Tile.Wang
import Util

import qualified Data.Text as T
import System.Environment
import System.Exit
import Text.Show.Pretty

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
  tss <- loadTileSets "data/tilesets.conf"
  (tsc,ts) <- loadTextureFromSets tss "fence"
  rTM <- runRandomIO $ randomTileMap (0,1) sz
  putStrLn $ ppTileMap rTM
  let wrc = defaultWangRenderConfig
  let wts = mkWangTextureSet wrc tsc ts wangTiles2x2
  displayWangTileMap wrc wts rTM
    
  -- (set,r,c) <- parseArgs
  -- (ts,cfg)  <- loadWangTileSet wangTiles2x2 =<< lookupSet set tsm
  -- tm <- ioWangTileMap ts (r,c)
  -- displayTileMap cfg ts tm

