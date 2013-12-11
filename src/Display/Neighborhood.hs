
module Display.Neighborhood where

import Tile
import Tile.Neighborhood
import Config.Render
import Config.TileSet
import Display

type NeighborhoodTextureSet = TextureSet Neighborhood

displayNeighborhoodTileMap :: RenderConfig
  -> NeighborhoodTextureSet -> Size -> TileMap -> IO ()
displayNeighborhoodTileMap = displayTileMap

mkNeighborhoodTextureSet :: TileSetConfig
  -> NeighborhoodTileSet -> NeighborhoodTextureSet
mkNeighborhoodTextureSet = mkTextureSet const

