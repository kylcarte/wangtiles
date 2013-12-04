
import Display
import Tile

import Data.Default
import Graphics.Gloss
import System.Environment

main :: IO ()
main = do
  [r,c] <- fmap (map read) getArgs
  tm <- ioWangTileMap def (r,c)
  displayTileMap 600 tm

