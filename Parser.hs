module Parser (version)
where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Bits
import Data.Word
import Data.Mysql

version :: Get Version
version = parsePacket $ do
   protocol <- fromIntegral `fmap` getWord8
   serverVersion <- L.toStrict `fmap` getLazyByteStringNul
   return $ Version protocol serverVersion


getWord24le :: Get Word32 -- TODO Word24 structure
getWord24le = do
    s <- fmap id $ getByteString 3
    return $! (fromIntegral (s `B.index` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
              (fromIntegral (s `B.index` 0) )

parsePacket :: Get a -> Get a
parsePacket action = do
  v <- do
    length <- getWord24le
    skip 1
    getLazyByteString $ fromIntegral length
  return $ runGet action v
