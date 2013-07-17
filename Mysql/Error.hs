module Mysql.Error
       (ThrowsError
       , die
       , dieS
       , assert
       , assertS
       , lift
       , liftThrows
       , toIO )
where

import Control.Monad.Error
import Control.Exception (try)

data MysqlError = Generic String deriving Show

instance Error MysqlError where
  noMsg = Generic "Unknown error"
  strMsg = Generic

type ThrowsError = ErrorT MysqlError

dieS :: Monad a => String -> ThrowsError a b
dieS = throwError . strMsg
assertS f s = assert f $ strMsg s

die :: Monad a => MysqlError -> ThrowsError a b
die = throwError

assert :: Monad a => Bool -> MysqlError -> ThrowsError a ()
assert f e = if f then die e else return ()

trapError :: (Show e, MonadError e m) => m String -> m String
trapError a = catchError a $ return . show

liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

toIO :: ThrowsError IO a -> IO a
toIO c = do
  res <- runErrorT c
  case res of
    Right v -> return v
