module Error ()
where

import Control.Monad.Error

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
