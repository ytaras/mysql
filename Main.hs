module Main (main)
where

import Network
import System.IO
import Data.ByteString.Lazy as L
import Data.Binary.Get
import Parser

main = do
  handle <- connection
  v <- execute handle version
  print v

connection = connectTo "localhost" $ PortNumber 3306

execute :: Handle -> Get a -> IO a
execute handler action = do
  string <- L.hGetContents handler
  return $ runGet action string
