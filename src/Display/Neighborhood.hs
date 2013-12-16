
module Display.Neighborhood where

import Config.Render
import Config.TileSet
import Data.TileMap
import Data.Points
import Display
import Tile.Neighborhood

displayNeighborhoodTileMap :: (Enum c) => RenderConfig
  -> NeighborhoodTextureSet -> Size c -> TileMap c -> IO ()
displayNeighborhoodTileMap = displayTileMap

mkNeighborhoodTextureSet :: TileSetConfig
  -> NeighborhoodTileSet -> NeighborhoodTextureSet
mkNeighborhoodTextureSet = mkTextureSet const

