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

type Log = [String]
type Action = WriterT Log (ReaderT Handle IO)
type Bytes = BS.ByteString

main = withSocketsDo $ perform

runAction :: Handle -> Action a -> IO a
runAction h action = runReaderT (runWriterT action) h >>=
  \(result, log) -> mapM_ putStrLn log >> return result

port = PortNumber 3306

perform :: IO Handle
perform = do
  handle <- connectTo "localhost" port
  runAction handle mainAction
  return handle

mainAction :: Action ()
mainAction = handshake

handshake = do
  version <- readWord8
  stringVersion <- readString
  return ()

readBytes :: Int -> Action Bytes
readBytes n = ask >>= \h -> liftIO $ BS.hGet h n


skip :: Int -> Action ()
skip n = do
  bytes  <- readBytes n
  tell $ ["Skipping " ++ (show n) ++  " bytes: " ++ show bytes]
  return ()

readWord8 :: Action Word8
readWord8 = do
  bytes <- readBytes 1
  let ret = BS.head bytes
  tell $ ["Reading word8 " ++ (show ret) ++ ", it was read from " ++ (show bytes)]
  return ret

readString :: Action Bytes
readString = evalStateT readString' []
             where
               readString' = do
                 byte <- lift $ readWord8
                 if byte == 0
                   then returnString
                   else modify (byte:) >> readString'
               returnString = do
                 bytes <- get
                 let string = BS.pack $ reverse $ bytes
                 tell $ ["Reading string " ++ show string ++ " from bytes" ++ show bytes]
                 return string
