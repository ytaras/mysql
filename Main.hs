module Main (main)
where

import Network
import System.IO
import Data.ByteString.Lazy as L
import Mysql.HighParser
import Mysql.Error

main = do
  handle <- connection
  v <- toIO $ execute handle version
  print v

connection = connectTo "localhost" $ PortNumber 3306

execute :: Handle -> Parser a -> ThrowsError IO a
execute handler action = do
  string <- lift $ L.hGetContents handler
  x <- executeParserM action string
  return x
