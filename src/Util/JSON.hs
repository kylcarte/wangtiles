
module Util.JSON where

import Error
import Util.HandleIO

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS

decodeFile :: A.FromJSON a => FilePath -> IO a
decodeFile f = io errMsg . errorDecode =<< BS.readFile f
  where
  errMsg = "couldn't parse file '" ++ f ++ "' to JSON"

errorDecode :: A.FromJSON a => BS.ByteString -> Error a
errorDecode = fromEither . A.eitherDecode

