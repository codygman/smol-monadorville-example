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

-- This is my App's instance of MonadOrville
instance MonadOrville HDBC.Connection (App (Pool HDBC.Connection) IO) where
  getOrvilleEnv = newOrvilleEnv <$> App ask
  runningQuery _ _ query = query -- error "implement runningQuery"
  localOrvilleEnv f = App
                    . local id -- is this fine?
                    . runApp

-- Is this what I need to write/somehow use?
instance MonadOrville HDBC.Connection (App (Pool HDBC.Connection) (CustomMonad IO)) where

-- https://taylor.fausak.me/orville/Database-Orville-Core.html#t:MonadOrville
loggingOrvilleExSelect :: (MonadFail m, Resource.MonadThrow m, MonadIO m, HDBC.IConnection conn, MonadOrville conn m) =>  LoggingOrville m [String]
loggingOrvilleExSelect = do
  LoggingOrville $ do
    selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (col @Text.Text "aggnumdirectargs")

main :: IO ()
main = do
  dbPool  <- liftIO $ createPool (HDBC.connectPostgreSQL  "postgresql://postgres@localhost:5432" ) HDBC.disconnect 1 60 1
  liftIO $ putStrLn "pool created"
  -- why does this small transformer stack use IO in my `loggingOrvilleExSelect` function and how can I make a custom instance for LoggingOrville that prints out the sql in `runningQuery`?

  print @[String] =<< (flip runReaderT dbPool . runApp . runLoggingOrville $ do
                           loggingOrvilleExSelect)

-- experiments farther below if interested

















  -- 1.
  -- print @[String] =<< (flip runReaderT dbPool . runApp . runLoggingOrville $ do
  --                         -- _1 :: LoggingOrville (App (Pool HDBC.Connection) IO) [String]
  --                          _1)

  -- 2.
  -- print @[String] =<< (flip runReaderT dbPool . runApp $ do
  --                         -- _2 :: _ :: App (Pool HDBC.Connection) IO [String]
  --                          _2)

  -- 3.
  -- print @[String] =<< (flip runReaderT dbPool $ do
  --                         -- _3 :: ReaderT (Pool HDBC.Connection) IO [String]
  --                          _3)

  -- what does lift do for these?
  -- print @[String] =<< (flip runReaderT dbPool . lift $ do
  --                         -- _4 :: IO [String]
  --                          _4
  --                    )
