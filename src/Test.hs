
import Display
import Tile

import Control.Applicative
import Data.List (intercalate)
import Graphics.Gloss
import System.Environment
import System.Exit
import System.FilePath
import qualified Data.Map as M

tileSetDirectory :: FilePath
tileSetDirectory = "data"

-- Tile set data -------------------------------------------------------------

tileSets :: M.Map String TileSetConfig
tileSets = M.fromList
  [ ( "dungeon"   , dungeon   )
  , ( "tubesocks" , tubesocks )
  ]

dungeon :: TileSetConfig
dungeon = TileSetConfig
  { pixels       = 16
  , textureFiles = files
  , fileChoice   = OrderedChoice
  }
  where
  files =
    [ tileSetDirectory </> "dungeon/wng-" ++ i <.> "bmp"
    | i <- map show [0..7]
    ]

tubesocks :: TileSetConfig
tubesocks = TileSetConfig
  { pixels       = 32
  , textureFiles = files
  , fileChoice   = RandomChoice
  }
  where
  files =
    [ tileSetDirectory </> "tubesocks/wang" ++ s <.> "bmp"
    | i <- map show [0..13]
    , let s = case i of
                [_] -> '0' : i
                _   -> i
    ]

------------------------------------------------------------------------------

parseArgs :: IO (String,Int,Int)
parseArgs = do
  as <- getArgs
  case as of
    [s,r,c] -> return (s,read r, read c)
    _       -> usage >> exitFailure

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn $ "  ./wangtiles <" ++ allSets ++ "> <# rows> <# cols>"
  where
  allSets = intercalate "|" $ M.keys tileSets

lookupSet :: String -> IO TileSetConfig
lookupSet s = case M.lookup s tileSets of
  Just tmpl -> return tmpl
  Nothing   -> do putStrLn $ "Error: unknown tile set '" ++ s ++ "'"
                  exitFailure

main :: IO ()
main = do
  (set,r,c) <- parseArgs
  (ts,cfg)  <- loadDefaultTileSet =<< lookupSet set
  tm <- ioWangTileMap ts (r,c)
  displayTileMap cfg tm

