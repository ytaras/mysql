module LowLevelParser
       ( nullTerminatedString
       , parsePacket
       , B.ByteString )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Attoparsec.ByteString.Lazy
import Prelude hiding (take, takeWhile)
import Data.Bits
import Data.Word
import Control.Applicative


nullTerminatedString :: Parser B.ByteString
nullTerminatedString = takeWhile ((==) 0)

getWord24le :: Parser Word32 -- TODO Word24 structure
getWord24le = do
    s <- take 3
    return $! (fromIntegral (s `B.index` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
              (fromIntegral (s `B.index` 0) )

skipWords :: Integral a => a -> Parser ()
skipWords n = take (fromIntegral n) >> return ()

parsePacket :: Parser a -> Parser a
parsePacket action = do
  len <- fromIntegral <$> getWord24le
  skipWords 1
  str <- L.fromStrict <$> take len
  case eitherResult $ parse action str of
    Right result -> return result
    Left err -> fail err
