{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Test where

import Data.Configurator
import Data.Configurator.Types
import Data.HashMap.Strict
import qualified Data.Map as M
import qualified Data.Text as T
import System.Exit
import System.FilePath

type TileSetMap = M.Map Name TileSetConfig

data TileSetConfig = TileSetConfig
  { tileWidth :: Float
  , textureFiles :: [FilePath]
  , fileChoice :: FileChoice
  } deriving (Eq,Show)

data FileChoice
  = OrderedChoice
  | RandomChoice
  deriving (Eq,Show)

testConfiguration :: IO ()
testConfiguration = do
  cfg <- getMap =<< load [Required $ "data" </> "tilesets" <.> "conf"]
  let gm = groupMaps cfg
  tsm <- foldrMWithKey configureTileSet M.empty gm

groupMaps :: HashMap Name Value -> M.Map Name (M.Map Name Value)
groupMaps = foldrWithKey processName M.empty
  where
  processName n v = M.alter updateSubMap gn
    where
    (gn,rest) = splitPrefix "." n
    updateSubMap mm = Just $ case mm of
      Nothing -> M.singleton rest v
      Just m  -> M.insert rest v m

foldrMWithKey :: (Ord k, Monad m) => (k -> v -> a -> m a) -> a -> M.Map k v -> m a
foldrMWithKey f a m
  | Just ((k,v),m') <- M.minViewWithKey m
  = do a' <- foldrMWithKey f a m'
       f k v a'
  | otherwise
  = return a

configureTileSet :: Name -> M.Map Name Value -> TileSetMap -> IO TileSetMap
configureTileSet n m tsm = do
  tw <- getField     "tile-width"
  r  <- getField     "random"
  ts <- getListField "textures"
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
  getListField f
    | Just vs <- convertList =<< M.lookup f m
    = return vs
    | otherwise
    = missingField f

convertList :: Configured a => Value -> Maybe [a]
convertList (List vs) = mapM convert vs
convertList _ = Nothing

splitPrefix :: T.Text -> T.Text -> (T.Text,T.Text)
splitPrefix s t = (t' , T.intercalate s ts)
  where
  t':ts = T.splitOn s t

