{-# LANGUAGE OverloadedStrings #-}

import Display
import Tile
import Config

import Control.Applicative
import Data.Configurator
import Data.Configurator.Types
import qualified Data.HashMap.Strict as H
import Data.List (intercalate)
import qualified Data.Text as T
import Graphics.Gloss
import System.Environment
import System.Exit
import System.FilePath
import qualified Data.Map as M

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
  tsm <- buildTileSetMap "data/tilesets.conf"
  (set,r,c) <- parseArgs
  (ts,cfg)  <- loadDefaultTileSet =<< lookupSet set tsm
  tm <- ioWangTileMap ts (r,c)
  displayTileMap cfg tm

