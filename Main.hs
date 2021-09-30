{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-methods #-}
module Main where

import qualified Control.Monad.Trans.Class as Trans
import qualified Database.Orville              as Orville
import qualified Database.Orville.Raw          as Orville
import qualified Control.Monad.Reader          as Reader
import qualified Control.Monad.Trans.Control   as Control
import qualified Control.Monad.Trans.Resource  as Resource
import qualified Control.Monad.Base            as Base
import qualified Database.HDBC.PostgreSQL      as Postgres
import qualified Database.PostgreSQL.Simple as Simple
import qualified Data.Pool as Pool
import qualified Database.HDBC      as HDBC
import qualified Data.Convertible as Convertible
import qualified Data.Text as Text

data EnvironmentWith context = EnvironmentWith
  { environmentContext :: context
  }

newtype App context m a =
  App { runApp :: Reader.ReaderT (EnvironmentWith context) m a }
  deriving
  ( Applicative
  , Functor
  , Monad
  , Base.MonadBase b
  , Control.MonadBaseControl b
  , Reader.MonadIO
  , MonadFail
  , Resource.MonadThrow
  )

newtype LoggingOrville c m a =
  LoggingOrville { runLoggingOrville :: m a }
  deriving (Applicative
           , Monad
           , Functor
           , MonadFail
           , Reader.MonadIO
           , Base.MonadBase b
           , Control.MonadBaseControl b
           , Resource.MonadThrow
           )

-- https://taylor.fausak.me/orville/Database-Orville-Core.html#t:MonadOrville
-- I can provide instance for my App monad
instance (HDBC.IConnection conn, Orville.MonadOrville conn m) => Orville.MonadOrville Postgres.Connection (App String m) where
  getOrvilleEnv = do
    dbPool  <- Reader.liftIO $ Pool.createPool (Postgres.connectPostgreSQL "postgresql://postgres@localhost:5432" ) (HDBC.disconnect) 1 60 1
    let env = Orville.newOrvilleEnv dbPool
    pure env
  localOrvilleEnv f =
    App
      . Reader.local
          (\localEnv -> localEnv)
      . runApp
  runningQuery _ sql query = do
    query

instance (Reader.MonadIO m, Resource.MonadThrow m, Control.MonadBaseControl IO m) => Orville.MonadOrville Postgres.Connection (LoggingOrville Postgres.Connection (App String m)) where
  getOrvilleEnv = do
    dbPool  <- Reader.liftIO $ Pool.createPool (Postgres.connectPostgreSQL "postgresql://postgres@localhost:5432" ) (HDBC.disconnect) 1 60 1
    let env = Orville.newOrvilleEnv dbPool
    pure env
  localOrvilleEnv f = id -- not sure why below doesn't work
    -- LoggingOrville
    --   . Reader.local
    --       (\localEnv -> localEnv)
    --   . runLoggingOrville
  runningQuery _ sql query = do
    error "hi"
    Reader.liftIO $ putStrLn sql
    query

runLoggingQuery :: (MonadFail m, Resource.MonadThrow m, Reader.MonadIO m, HDBC.IConnection conn, Orville.MonadOrville conn m) =>  LoggingOrville Postgres.Connection m [String]
runLoggingQuery = do
  LoggingOrville $ do
    Orville.selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (Orville.col @Text.Text "aggnumdirectargs")

runNormalQuery :: (MonadFail m, Resource.MonadThrow m, Reader.MonadIO m, HDBC.IConnection conn, Orville.MonadOrville conn m) =>  App String m [String]
runNormalQuery = do
  App $ do
    Orville.selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (Orville.col @Text.Text "aggnumdirectargs")

main :: IO ()
main = do
  dbPool  <- Reader.liftIO $ Pool.createPool (Postgres.connectPostgreSQL "postgresql://postgres@localhost:5432" ) (HDBC.disconnect) 1 60 1
  Reader.liftIO $ putStrLn "pool created"
  let env = Orville.newOrvilleEnv dbPool
  --  print =<< Reader.runReaderT ((flip Orville.runOrville env) . runApp runNormalQuery) (EnvironmentWith "tst")
  Reader.liftIO $  putStrLn "env created"

   -- ugh this one is using the default implementation :/
  print @[String] =<< (flip Orville.runOrville env . flip Reader.runReaderT (EnvironmentWith "") . runApp) (Orville.withTransaction runNormalQuery)

  -- This one is using the one that isn't supposed to be logging
  -- so it worked by accident
  print @[String] =<< (flip Orville.runOrville env . flip Reader.runReaderT (EnvironmentWith ("" :: String)) . runApp . runLoggingOrville) (Orville.withTransaction runLoggingQuery)
