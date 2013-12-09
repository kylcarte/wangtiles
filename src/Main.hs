{-# LANGUAGE OverloadedStrings #-}

import Display.Wang
import Tile.Wang
import Config.TileSet
import Config.TileSet.Wang

import Data.Configurator.Types
import qualified Data.Text as T
import System.Environment
import System.Exit

parseArgs :: IO (Name,Int,Int)
parseArgs = do
  as <- getArgs
  case as of
    [s,r,c] -> return (T.pack s,read r, read c)
    _       -> usage >> exitFailure

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn $ "  ./wangtiles <tileset> <# rows> <# cols>"

main :: IO ()
main = do
  tsm <- loadTileSetMap "data/tilesets.conf"
  (set,r,c) <- parseArgs
  (ts,cfg)  <- loadWangTileSet wangTiles2x2 =<< lookupSet set tsm
  tm <- ioWangTileMap ts (r,c)
  displayTileMap cfg ts tm

