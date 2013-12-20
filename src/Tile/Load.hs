
module Tile.Load where

import Data.Points
import Data.TileMap
import Data.TileMaps
import Error
import Tile.Legend
import Util
import Util.HandleIO

import Control.Applicative
import Data.Traversable (sequenceA)
import Data.Text (Text)

data TileData c = TileData
  { baseTileMap :: TileMap c
  , baseLegend  :: Legend
  , baseSize    :: Size c
  , dataLayers  :: TileMaps c
  } deriving (Eq,Show)

buildTileData :: (CoordType c) => FilePath
  -> [(Text,Legend -> TileMap c -> IO (TileMap c))]
  -> IO (TileData c)
buildTileData fp fs = do
  (tm,leg) <- readRoom fp
  ms       <- mapM (runOne leg tm) fs
  return TileData
    { baseTileMap = tm
    , baseLegend  = leg
    , baseSize    = tmSize tm
    , dataLayers  = tmsFromList ms
    }
  where
  runOne leg tm (n,f) = (,) n <$> f leg tm

buildTileMaps :: (CoordType c) => [(Text,IO (TileMap c))] -> IO (TileMaps c)
buildTileMaps ms = tmsFromList <$> mapM loadOne ms
  where
  loadOne = sequenceA

withLegend :: HandleIO m => (Legend -> TileMap c -> m (TileMap c))
  -> Legend -> TileMap c -> IO (TileMap c)
withLegend f leg = io' . f leg

tdLookupMap :: (CoordType c) => TileData c -> Text -> Error (TileMap c)
tdLookupMap = tmsLookup . dataLayers

tileIndexAt :: (CoordType c) => TileData c -> Coord c -> Text -> Error TileIndex
tileIndexAt td cd nm = do
  tm <- tdLookupMap td nm
  tmLookup tm cd

tdLegendName :: TileData c -> TileIndex -> Error Text
tdLegendName = legendName . baseLegend

-- Pretty Printing {{{

ppTileData :: (CoordType c) => TileData c -> String
ppTileData td = ppLines
  [ "Base Layer:"
  , ppTileMap  $ baseTileMap td
  , "Legend:"
  , ppLegend   $ baseLegend  td
  , "Base Size:"
  , ppSize     $    baseSize td
  , "Data Layers:"
  , ppTileMaps $  dataLayers td
  ]

-- }}}

