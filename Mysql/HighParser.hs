module Mysql.HighParser
       ( version
       , executeParserM
       , Parser)
where

import Mysql.Data
import Data.Attoparsec.ByteString.Lazy
import Prelude hiding (take, takeWhile)
import Control.Monad.Trans
import Mysql.Error
import Mysql.LowParser

version :: Parser Version
version = parsePacket $ do
   protocol <- fromIntegral `fmap` anyWord8
   serverVersion <- nullTerminatedString
   return $ Version protocol serverVersion
