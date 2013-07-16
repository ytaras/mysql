module Bytes (Bytes, toWord32)
where

import qualified Data.ByteString as BS
import Data.Word
import Data.Bits

type Bytes = BS.ByteString

toWord32 :: Bytes -> Word32
toWord32 a = BS.foldl' accum 0 $ BS.reverse a
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o
