module Mysql (main)
where

import Network
import System.IO
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Data.ByteString as BS

type Log = [String]
type Action = WriterT Log (ReaderT Handle IO)


main = withSocketsDo $ perform

port = PortNumber 3306

perform :: IO Handle
perform = do
  handle <- connectTo "localhost" port
  runAction handle mainAction
  return handle

mainAction :: Action ()
mainAction = do
  skip 1
  skip 4

readBytes :: Int -> Action ByteString
readBytes n = ask >>= \h -> liftIO $ BS.hGet h n

runAction :: Handle -> Action a -> IO a
runAction h action = runReaderT (runWriterT action) h >>=
  \(result, log) -> print log >> return result

skip :: Int -> Action ()
skip n = do
  bytes  <- readBytes n
  tell $ ["Skipping " ++ (show n) ++  " bytes: " ++ show bytes]
  return ()
