{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tile.Legend where

import Data.Points
import Data.TileMap
import Data.TileSet
import Error
import Util
import Util.HandleIO

import Control.Applicative
import Control.Arrow (second)
import qualified Data.Foldable as F
import Data.Traversable (traverse)
import qualified Data.Map as M
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T

type LegendEntry = Text

type Room = TileMap

data Legend = Legend
  { legendTo   :: M.Map LegendEntry TileIndex
  , legendFrom :: TileSet LegendEntry
  } deriving (Eq,Show)

legendIndex :: (Monad m) => Legend -> Text -> ErrorT m TileIndex
legendIndex leg n = reportNothing "legendIndex" $ M.lookup n $ legendTo leg

legendName :: (Monad m) => Legend -> TileIndex -> ErrorT m Text
legendName leg ti = wrapFail "legendName" $ tsLookup (legendFrom leg) ti

-- Parsing {{{

readRoom :: (CoordType c) => FilePath
  -> IO (Room c,Legend)
readRoom f = io err . parse =<< T.readFile f
  where
  parse :: (CoordType c) => Text -> Error (Room c, Legend)
  parse = 
      parseRoomLegend
    . splitLegend
    . removeComments
  err = fail "Could not parse map legend"

splitLegend :: [Text] -> ([Text],[Text])
splitLegend = break (T.isPrefixOf "*")

removeComments :: Text -> [Text]
removeComments = filter (not . T.isPrefixOf "#") . T.lines

parseRoomLegend :: (CoordType c) => ([Text],[Text]) -> Error (Room c,Legend)
parseRoomLegend (r,l) = (,) fp <$> parseLegend l ts
  where
  (fp,ts) = parseRoom r

-- }}}

-- Printing {{{

printRoom :: FilePath -> IO ()
printRoom f = do
  r <- readRoom f
  printRoomLegend (r :: (Room Int,Legend))

printRoomLegend :: (CoordType c)
  => (Room c,Legend) -> IO ()
printRoomLegend (r,l) = do
  T.putStrLn "Room: "
  putStrLn $ ppTileMap r
  T.putStrLn "Legend: "
  disp l

-- }}}

-- Room {{{

type RoomBuilder c = (Room c,ParseTileSet)
type RoomParse c   = (Room c,RBuilder)
type ParseTileSet  = [(TileIndex,Char)]

parseRoom :: (CoordType c) => [Text] -> RoomBuilder c
parseRoom = second mkParseTileSet . foldr fn empties . zip [0..]
  where
  empties = (emptyTileMap,emptyRBuilder)
  fn (r,l) rp = parseRoomLine r rp l

parseRoomLine :: (CoordType c) => c -> RoomParse c
  -> Text -> RoomParse c
parseRoomLine r rp = foldr fn rp . zip [0..] . unpack
  where
  fn (c,ch) (fp,bld) = (fp',bld')
    where
    (ti,bld') = rbldInsert ch bld
    fp' = tmInsert (mkCoord c r) ti fp

mkParseTileSet :: [(Char,Int)] -> ParseTileSet
mkParseTileSet = map swap2



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

parseLegend :: [Text] -> ParseTileSet -> Error Legend
parseLegend l ts = do
  bld  <- mkLegendBuilder l
  from <- mapM (traverse (`lbldLookup` bld)) ts
  let to = map swap2 from
  return Legend
    { legendTo   = M.fromList to
    , legendFrom = tsFromList from
    }

mkLegendBuilder :: [Text] -> Error LBuilder
mkLegendBuilder = F.foldrM fn emptyLBuilder
  where
  -- TODO : better error reporting
  fn l bld
    | l0 <- T.strip l
    , "*" `T.isPrefixOf` l0
    , l1 <- T.stripStart $ T.drop 1 l0
    , Just (c,l2) <- second T.stripStart <$> T.uncons l1
    , ":" `T.isPrefixOf` l2
    , t <- T.stripStart $ T.drop 1 l2
    , not $ T.null t
    = return $ lbldInsert c t bld
    | otherwise
    = fail $ "Couldn't parse Legend entry: " ++ unpack l


type LBuilder = M.Map Char Text

emptyLBuilder :: LBuilder
emptyLBuilder = M.empty

lbldLookup :: Char -> LBuilder -> Error Text
lbldLookup c = reportNothing "lbldLookup" . M.lookup c

lbldInsert :: Char -> Text -> LBuilder -> LBuilder
lbldInsert = M.insert

-- }}}

-- Pretty Printing {{{

ppLegend :: Legend -> String
ppLegend = ppTileSet . legendFrom

-- }}}

