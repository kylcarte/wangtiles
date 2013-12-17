
module Display.Neighborhood where

import Config.Render
import Config.TileSet
import Data.TileMap
import Data.TileSet
import Data.Points
import Display
import Texture
import Tile.Neighborhood

import Graphics.Gloss.Juicy

displayNeighborhoodTileMap :: (Enum c) => RenderConfig
  -> NeighborhoodTextureSet -> Size c -> TileMap c -> IO ()
displayNeighborhoodTileMap = displayTileMap

mkNeighborhoodTextureSet :: TileSetConfig
  -> TileSet Texture -> NeighborhoodTileSet -> NeighborhoodTextureSet
mkNeighborhoodTextureSet = mkTextureSet $ \t _ -> fromImageRGBA8 t

