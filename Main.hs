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

newtype App context m a = App {
  runApp :: Reader.ReaderT (EnvironmentWith context) m a
  } deriving
  ( Applicative
  , Functor
  , Monad
  , Base.MonadBase b
  , Control.MonadBaseControl b
  , Reader.MonadIO
  , MonadFail
  , Resource.MonadThrow
  )

newtype LoggingOrville c m a = LoggingOrville { runLoggingOrville :: Orville.OrvilleT c m a } deriving (Applicative, Monad, Functor, MonadFail, Reader.MonadIO, Base.MonadBase b, Control.MonadBaseControl b)

-- https://taylor.fausak.me/orville/Database-Orville-Core.html#t:MonadOrville
instance (Monad m, Reader.MonadIO (LoggingOrville Postgres.Connection m), Control.MonadBaseControl IO (LoggingOrville Postgres.Connection m)) => Orville.MonadOrville Postgres.Connection (LoggingOrville Postgres.Connection m) where
  getOrvilleEnv = Orville.getOrvilleEnv
  runningQuery _ sql query = do
    Reader.liftIO $ putStrLn sql
    query

runNormalQuery :: App String IO [String]
runNormalQuery = do
  dbPool  <- Reader.liftIO $ Pool.createPool (Postgres.connectPostgreSQL "postgresql://postgres@localhost:5432" ) (HDBC.disconnect) 1 60 1
  Reader.liftIO $ putStrLn "pool created"
  let env = Orville.newOrvilleEnv dbPool
  Reader.liftIO $  putStrLn "env created"
  flip Orville.runOrville env $ do
    Orville.selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 1;"
      []
      (Orville.col @Text.Text "aggnumdirectargs")

runLoggedQuery :: App String IO [String]
runLoggedQuery = do
  dbPool  <- Reader.liftIO $ Pool.createPool (Postgres.connectPostgreSQL "postgresql://postgres@localhost:5432" ) (HDBC.disconnect) 1 60 1
  Reader.liftIO $ putStrLn "pool created"
  let env = Orville.newOrvilleEnv dbPool
  Reader.liftIO $  putStrLn "env created"

  -- This freezes... nested transactions?
  -- core of problem is how to do:
  -- _ :: LoggingOrville Postgres.Connection IO [String] -> App String IO [String]
  -- I just want the base IO monad to be used... right? But I want the LoggingOrville `runningQuery` called
  _ . Orville.withTransaction @Postgres.Connection @(LoggingOrville (Postgres.Connection) IO) $ LoggingOrville $ do
        Orville.selectSql @String
          "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 1;"
          []
          (Orville.col @Text.Text "aggnumdirectargs")

  -- new attempt
  -- This doesn't even run?
  -- flip Orville.runOrville env . runLoggingOrville . Orville.withTransaction $ newApproach

main :: IO ()
main = do

  putStrLn "start"
  print =<< (Reader.runReaderT @_ @IO @_) (runApp runNormalQuery) (EnvironmentWith "tst")
  putStrLn "done"

  putStrLn "start"
  -- print =<< (Reader.runReaderT @_ @IO @_) (runApp runLoggedQuery) (EnvironmentWith "tst")
  putStrLn "done"

  -- Reader.void $ Reader.runReaderT (runApp trivialAppMonadUsage) (EnvironmentWith "something")





-- other stuff I tried

-- sanity check
trivialAppMonadUsage :: App String IO ()
trivialAppMonadUsage = do
  x :: String <- App . Reader.asks $ environmentContext
  Reader.liftIO $ putStrLn x


-- try it backwards basically... pretty sure it's not right (also freezes)
newApproach :: LoggingOrville Postgres.Connection (App String IO) [String]
newApproach = Orville.withTransaction @Postgres.Connection @(LoggingOrville (Postgres.Connection) _) $ LoggingOrville $ do
  Orville.selectSql @String
          "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 1;"
          []
          (Orville.col @Text.Text "aggnumdirectargs")
