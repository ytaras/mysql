module Mysql (main)
where

import Network
import System.IO
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Bytes

type Log = [LogRecord]
data LogLevel = Bytes | Structures | Logic
              deriving Show
type LogRecord = (LogLevel, String)

logB s = tell [(Bytes, s)]
logS s = tell [(Structures, s)]
logL s = tell [(Logic, s)]

type Action = WriterT Log (ReaderT Handle IO)

main = withSocketsDo $ perform

runAction :: Handle -> Action a -> IO a
runAction h action = runReaderT (runWriterT action) h >>=
  \(result, log) -> mapM_ print log >> return result

port = PortNumber 3306

perform :: IO Handle
perform = do
  handle <- connectTo "localhost" port
  runAction handle mainAction
  return handle

mainAction :: Action ()
mainAction = handshake >> handshake

handshake = do
  ver <- readPacket
  logL $ "Version is " ++ show ver
  return ()

readBytes :: Int -> Action Bytes
readBytes n = do
  h <- ask
  res <- liftIO $ BS.hGet h n
  logB $ "Read " ++ show n ++ " bytes: "  ++ show res
  return res

skip :: Int -> Action ()
skip n = do
  bytes  <- readBytes n
  logB $ "Skipping " ++ (show n) ++  " bytes: " ++ show bytes
  return ()

readWord32 :: Action Word32
readWord32 = do
  word32 <- toWord32 `fmap` readBytes 4
  logS $ "Reading word32 " ++ show word32
  return word32

readWord24 :: Action Word32 -- TODO Separate type
readWord24 = do
  word24 <- toWord32 `fmap` readBytes 3
  logS $ "Reading word24 " ++ show word24
  return word24

readPacket :: Action Bytes
readPacket = do
  length <- readWord24
  logS $ "Received packet length " ++ show length
  skip 1
  readBytes $ fromIntegral length
