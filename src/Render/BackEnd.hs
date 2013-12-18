{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Render.BackEnd where

import Data.Points
import Texture

class BackEnd extra b | b -> extra where
  display :: extra -> b -> IO ()
  renderTexture :: extra -> Coord Int -> Texture -> b
  layers :: extra -> [b] -> b

