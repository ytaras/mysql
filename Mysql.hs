module Mysql (main)
where

import Network
import System.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Get
import Data.Bits
import Control.Monad.Writer

type Log = [LogRecord]
data LogLevel = Bytes | Structures | Logic
              deriving Show
type LogRecord = (LogLevel, String)

logB s = tell [(Bytes, s)]
logS s = tell [(Structures, s)]
logL s = tell [(Logic, s)]


main = do
  handle <- connection
  ver <- execute handle getWord24le
  print ver

connection = connectTo "localhost" $ PortNumber 3306

execute :: Handle -> Get a -> IO a
execute handler action = do
  string <- L.hGetContents handler
  return $ runGet action string

readPacket :: Get B.ByteString
readPacket = do
  version <- getWord24le
  return $ B.empty


getWord24le :: Get Word32 -- TODO Word24 structure
getWord24le = do
    s <- fmap id $ getByteString 3
    return $! (fromIntegral (s `B.index` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
              (fromIntegral (s `B.index` 0) )
