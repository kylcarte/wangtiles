
module Config.Render.Wang where

import Config.Render

data WangRenderConfig = WangRenderConfig
  { wEdgeRadius   :: Float -> Float
  , wRenderEdges  :: Bool
  , wRenderConfig :: RenderConfig
  }

defaultWangRenderConfig :: WangRenderConfig
defaultWangRenderConfig = WangRenderConfig
  { wEdgeRadius   = (/ 2)
  , wRenderEdges  = True
  , wRenderConfig = defaultRenderConfig
  }

