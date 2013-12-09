
module Config.Render.Wang where

import Config.Render

data WangRenderConfig = WangRenderConfig
  { wTileSize     :: (Float,Float)
  , wEdgeRadius   :: Float -> Float
  , wRenderEdges  :: Bool
  , wRenderConfig :: RenderConfig
  }

wTileRadius :: WangRenderConfig -> (Float,Float)
wTileRadius cfg = (x / 2, y / 2)
  where
  (x,y) = wTileSize cfg

defaultWangRenderConfig :: WangRenderConfig
defaultWangRenderConfig = WangRenderConfig
  { wTileSize     = (16,16)
  , wEdgeRadius   = (/ 2)
  , wRenderEdges  = True
  , wRenderConfig = defaultRenderConfig
  }

