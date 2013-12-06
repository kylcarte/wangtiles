{-# LANGUAGE OverloadedStrings #-}

module Config where

import Display
import Tile

import Control.Applicative
import Data.Configurator
import Data.Configurator.Types
import qualified Data.HashMap.Strict as H
import Data.List (intercalate,sort)
import qualified Data.Text as T
import Graphics.Gloss
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.Glob
import qualified Data.Map as M

-- Tile set data -------------------------------------------------------------

type TileSetMap = M.Map Name TileSetConfig

lookupSet :: Name -> TileSetMap -> IO TileSetConfig
lookupSet n tsm = case M.lookup n tsm of
  Just cfg  -> return cfg
  Nothing   -> do putStrLn $ "Error: unknown tile set '" ++ T.unpack n ++ "'"
                  exitFailure

groupMaps :: H.HashMap Name Value -> M.Map Name (M.Map Name Value)
groupMaps = H.foldrWithKey processName M.empty
  where
  processName n v = M.alter updateSubMap gn
    where
    (gn,rest) = splitPrefix "." n
    updateSubMap mm = Just $ case mm of
      Nothing -> M.singleton rest v
      Just m  -> M.insert rest v m

configureTileSet :: Name -> M.Map Name Value -> TileSetMap -> IO TileSetMap
configureTileSet n m tsm = do
  tw  <- getField "tile-width"
  r   <- getField "random"
  tsg <- getField "textures"
  ts  <- sort <$> glob tsg
  let conf = TileSetConfig
               { tileWidth = tw
               , textureFiles = ts
               , fileChoice = if r then RandomChoice else OrderedChoice
               }
  return $ M.insert n conf tsm
  where
  missingField f = do
    putStrLn $ "TileSet '"   ++ T.unpack n ++
      "' is missing field '" ++ T.unpack f ++ "'"
    exitFailure
  getField f
    | Just v <- convert =<< M.lookup f m
    = return v
    | otherwise
    = missingField f

buildTileSetMap :: FilePath -> IO TileSetMap
buildTileSetMap f = do
  cfg <- getMap =<< load [Required f]
  let gm = groupMaps cfg
  foldrMWithKey configureTileSet M.empty gm

-- Helpers

foldrMWithKey :: (Ord k, Monad m) => (k -> v -> a -> m a) -> a -> M.Map k v -> m a
foldrMWithKey f a m
  | Just ((k,v),m') <- M.minViewWithKey m
  = do a' <- foldrMWithKey f a m'
       f k v a'
  | otherwise
  = return a

splitPrefix :: T.Text -> T.Text -> (T.Text,T.Text)
splitPrefix s t = (t' , T.intercalate s ts)
  where
  t':ts = T.splitOn s t

