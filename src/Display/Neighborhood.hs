
module Display.Neighborhood where

import Config.Render
import Config.TileSet
import Display
import Tile
import Tile.Neighborhood

displayNeighborhoodTileMap :: RenderConfig
  -> NeighborhoodTextureSet -> TileMap -> IO ()
displayNeighborhoodTileMap = displayTileMap

mkNeighborhoodTextureSet :: TileSetConfig
  -> NeighborhoodTileSet -> NeighborhoodTextureSet
mkNeighborhoodTextureSet = mkTextureSet const

