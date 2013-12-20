{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Render.BackEnd where

import Data.TileMap
import Data.Points
import Error
import Texture
import Tile.Load
import Util

class BackEnd config where
  type Picture config
  display :: config -> Error (Picture config) -> IO ()
  renderTextureAt    :: (CoordType c) => config
    -> Coord c -> Texture -> Error (Picture config)
  blankPicture       :: config -> Picture config
  pictures           :: config -> [Picture config] -> Picture config

  displayBaseTileMap :: (CoordType c)
    => config -> TileData c
    -> (TileIndex -> Coord c -> Error Texture) -> IO ()
  displayBaseTileMap cfg td texFn = display cfg
    (renderBaseTileMap cfg td texFn :: Error (Picture config))

  renderBaseTileMap  :: (CoordType c)
    => config -> TileData c
    -> (TileIndex -> Coord c -> Error Texture) -> Error (Picture config)
  renderBaseTileMap cfg td texFn =
    tmFoldrWithKey textureOne (return $ blankPicture cfg)
      $ baseTileMap td
    where
    textureOne cd ti mp = do
      tx <- texFn ti cd
      p1 <- mp
      p2 <- renderTextureAt cfg cd tx
      return $ pictures cfg [p1,p2]

