
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

tileSets :: M.Map String TextureTemplate
tileSets = M.fromList
  [ ( "dungeon"   , dungeonTmpl   )
  , ( "tubesocks" , tubesocksTmpl )
  ]

dungeonTmpl :: TextureTemplate
dungeonTmpl n = return (16,files)
  where
  files =
    [ tileSetDirectory </> "dungeon/wng-" ++ i <.> "bmp"
    | i <- map show [0..n-1]
    ]

tubesocksTmpl :: TextureTemplate
tubesocksTmpl n = (,) 32 <$> choose n files
  where
  files =
    [ tileSetDirectory </> "tubesocks/wang" ++ s <.> "bmp"
    | i <- map show [0..n-1]
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

lookupSet :: String -> IO TextureTemplate
lookupSet s = case M.lookup s tileSets of
  Just tmpl -> return tmpl
  Nothing   -> do putStrLn $ "Error: unknown tile set '" ++ s ++ "'"
                  exitFailure

main :: IO ()
main = do
  (set,r,c) <- parseArgs
  tmpl      <- lookupSet set
  (ts,cfg)  <- loadDefaultTileSet tmpl
  tm <- ioWangTileMap ts (r,c)
  displayTileMap cfg tm

