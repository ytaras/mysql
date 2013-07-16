module Data.Mysql (Version(..))
where

import Data.ByteString

data Version = Version { protocol :: Int
                       , serverVersion :: ByteString
                       } deriving Show
