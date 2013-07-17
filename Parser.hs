module Parser (version)
where

import Data.Mysql
import Data.Attoparsec.ByteString.Lazy
import Prelude hiding (take, takeWhile)
import Control.Monad.Trans
import Error
import LowLevelParser

version :: Parser Version
version = parsePacket $ do
   protocol <- fromIntegral `fmap` anyWord8
   serverVersion <- nullTerminatedString
   return $ Version protocol serverVersion
