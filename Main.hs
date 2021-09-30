{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-methods #-}
module Main where

import Control.Monad.Trans.Class as Trans
import Database.Orville
import Database.Orville.Raw
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource  as Resource
import Control.Monad.Base
import qualified Database.HDBC.PostgreSQL as HDBC
import qualified Database.PostgreSQL.Simple as Postgres
import Data.Pool
import qualified Database.HDBC as HDBC
import Data.Convertible
import qualified Data.Text as Text

newtype App dbPool m a =
  App { runApp :: ReaderT dbPool m a }
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadBase b
  , MonadBaseControl b
  , MonadIO
  , MonadFail
  , MonadThrow
  , MonadTrans
  )

newtype LoggingOrville m a =
  LoggingOrville { runLoggingOrville :: m a }
  deriving (Applicative
           , Monad
           , Functor
           , MonadFail
           , MonadIO
           , MonadBase b
           , MonadBaseControl b
           , Resource.MonadThrow
           )

instance MonadOrville HDBC.Connection (LoggingOrville (App HDBC.Connection IO)) where
    getOrvilleEnv = error "implement getOrvilleEnv"
    localOrvilleEnv f = error "implement localOrvilleEnv"
    runningQuery _ sql query = error "implement runningQuery"

-- https://taylor.fausak.me/orville/Database-Orville-Core.html#t:MonadOrville
loggingOrvilleExSelect :: (MonadFail m, Resource.MonadThrow m, MonadIO m, HDBC.IConnection conn, MonadOrville conn m) =>  LoggingOrville m [String]
loggingOrvilleExSelect = do
  LoggingOrville $ do
    selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (col @Text.Text "aggnumdirectargs")

loggingAppExSelect :: (LoggingOrville (App (Pool HDBC.Connection) IO)) [String]
loggingAppExSelect = loggingAppExSelect

main :: IO ()
main = do
  dbPool  <- liftIO $ createPool (HDBC.connectPostgreSQL  "postgresql://postgres@localhost:5432" ) HDBC.disconnect 1 60 1
  liftIO $ putStrLn "pool created"
  print @[String] =<< (flip runReaderT dbPool . runApp $ do
    runLoggingOrville loggingAppExSelect)
