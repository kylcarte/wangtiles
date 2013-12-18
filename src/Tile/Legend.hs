{-# LANGUAGE OverloadedStrings #-}

module Tile.Legend where

import Data.Points
import Data.TileMap
import Data.TileSet
import Util

import Control.Applicative
import Control.Arrow (second)
import Data.Text (Text)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Legend = Legend
  { legend :: Text
  } deriving (Eq,Show)

mkLegend :: Text -> Legend
mkLegend = Legend

type Room = TileMap
type LegendSet = TileSet Legend

-- Parsing {{{

readRoom :: (Integral c) => FilePath
  -> IO (Room c,LegendSet)
readRoom f = maybe err return . parse =<< T.readFile f
  where
  parse = 
      parseRoomLegend
    . splitLegend
    . removeComments
  err = fail "Could not parse map legend"

splitLegend :: [Text] -> ([Text],[Text])
splitLegend = break (T.isPrefixOf "*")

removeComments :: Text -> [Text]
removeComments = filter (not . T.isPrefixOf "#") . T.lines

parseRoomLegend :: (Integral c) => ([Text],[Text]) -> Maybe (Room c,LegendSet)
parseRoomLegend (r,l) = (,) fp <$> parseLegend l ts
  where
  (fp,ts) = parseRoom r

-- }}}

-- Printing {{{

printRoom :: FilePath -> IO ()
printRoom f = do
  r <- readRoom f
  printRoomLegend (r :: (Room Int,LegendSet))

printRoomLegend :: (Integral c, Show c)
  => (Room c,LegendSet) -> IO ()
printRoomLegend (r,l) = do
  T.putStrLn "Room: "
  putStrLn $ ppTileMap r
  T.putStrLn "Legend: "
  disp l

-- }}}

-- Room {{{

type RoomBuilder c = (Room c,ParseTileSet)
type RoomParse c   = (Room c,RBuilder)
type ParseTileSet  = TileSet Char

parseRoom :: (Integral c) => [Text] -> RoomBuilder c
parseRoom = second mkParseTileSet . foldr fn empties . zip [0..]
  where
  empties = (emptyTileMap,emptyRBuilder)
  fn (r,l) rp = parseRoomLine r rp l

mkParseTileSet :: RBuilder -> ParseTileSet
mkParseTileSet = tsFromList . map swap2

parseRoomLine :: (Integral c) => c -> RoomParse c
  -> Text -> RoomParse c
parseRoomLine r rp = foldr fn rp . zip [0..] . T.unpack
  where
  fn (c,ch) (fp,bld) = (fp',bld')
    where
    (ti,bld') = rbldInsert ch bld
    fp' = tmInsert (mkCoord c r) ti fp



type RBuilder = [(Char,TileIndex)]

emptyRBuilder :: RBuilder
emptyRBuilder = []

rbldLookup :: Char -> RBuilder -> Maybe TileIndex
rbldLookup = lookup

rbldInsert :: Char -> RBuilder -> (TileIndex,RBuilder)
rbldInsert ch bld = case rbldLookup ch bld of
  Nothing -> (n,(ch,n):bld)
  Just i  -> (i,bld)
  where
  n = length bld

-- }}}

-- Legend {{{

parseLegend :: [Text] -> ParseTileSet -> Maybe LegendSet
parseLegend l ts = do
  bld <- mkLegendBuilder l
  T.traverse (fmap mkLegend . (`lbldLookup` bld)) ts

mkLegendBuilder :: [Text] -> Maybe LBuilder
mkLegendBuilder = F.foldrM fn emptyLBuilder
  where
  -- XXX : better errors
  fn l bld
    | l0 <- T.strip l
    , T.isPrefixOf "*" l0
    , l1 <- T.stripStart $ T.drop 1 l0
    , Just (c,l2) <- second T.stripStart <$> T.uncons l1
    , T.isPrefixOf ":" l2
    , t <- T.stripStart $ T.drop 1 l2
    , not $ T.null t
    = Just $ lbldInsert c t bld
    | otherwise
    = Nothing



type LBuilder = M.Map Char Text

emptyLBuilder :: LBuilder
emptyLBuilder = M.empty

lbldLookup :: Char -> LBuilder -> Maybe Text
lbldLookup = M.lookup

lbldInsert :: Char -> Text -> LBuilder -> LBuilder
lbldInsert = M.insert

-- }}}

