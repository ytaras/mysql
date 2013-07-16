module Main (main)
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
  version <- execute handle versionPacket
  print version

connection = connectTo "localhost" $ PortNumber 3306

execute :: Handle -> Get a -> IO a
execute handler action = do
  string <- L.hGetContents handler
  return $ runGet action string

packet :: Get B.ByteString
packet = do
  length <- getWord24le
  skip 1
  getByteString $ fromIntegral length

data Version = Version { protocol :: Int
                         , serverVersion :: L.ByteString }
               deriving Show

version :: Get Version
version = do
   protocol <- fromIntegral `fmap` getWord8
   serverVersion <- getLazyByteStringNul
   return $ Version protocol serverVersion

versionPacket :: Get Version
versionPacket = do
  v <- packet
  return $ runGet version (L.fromStrict v)

getWord24le :: Get Word32 -- TODO Word24 structure
getWord24le = do
    s <- fmap id $ getByteString 3
    return $! (fromIntegral (s `B.index` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
              (fromIntegral (s `B.index` 0) )
