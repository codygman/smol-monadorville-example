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


newtype AppDebugSql m a =
  AppDebugSql { runAppDebugSql :: m a }
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
  runningQuery _ _ query = error "implement runningQuery"
  localOrvilleEnv f = App
                    . local id -- is this fine?
                    . runApp

{-
question: I have a working App monad using the ReaderT design pattern. Right now it works with a database library called Orville and (I'm pretty sure) uses the base Monad IO whose `runningQuery` function does not print out the sql query run. My custom Monad is simply called `AppDebugSql`, which of these instance heads look right for using it if any for `class MonadOrville conn m`:

1) (App (Pool HDBC.Connection) (AppDebugSql IO))
2) AppDebugSql (App (Pool HDBC.Connection) IO)

shortened version of question:

Using the ReaderT design pattern should Monads you want to behave differently in your `AppM` be the base monad or use your `AppM` as a base monad in instance heads? Assuming we have a `AppM ReaderT c m a` and a variant `AppMDebug m a` that has slightly different typeclass instances  which is correct 1) AppM (ReaderT c (AppMDebug IO)) 2) AppMDebug (AppM (ReaderT c m)). Here is a longer version of this question with specifics that is compiling and running (but not logging):

-}

-- Is this what I need to write/somehow use? Is this the right approach to specify my own `runningQuery` for `AppDebugSql`?
instance MonadOrville HDBC.Connection (App (Pool HDBC.Connection) (AppDebugSql IO)) where
  getOrvilleEnv = error "1"

-- https://taylor.fausak.me/orville/Database-Orville-Core.html#t:MonadOrville
loggingOrvilleExSelect :: (MonadFail m, Resource.MonadThrow m, MonadIO m, HDBC.IConnection conn, MonadOrville conn m) =>  AppDebugSql m [String]
loggingOrvilleExSelect = do
  AppDebugSql $ do
    selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (col @Text.Text "aggnumdirectargs")


-- f :: (MonadFail m, Resource.MonadThrow m, MonadIO m, HDBC.IConnection conn, MonadOrville conn m) =>  _ -> AppDebugSql m [String] -> IO [String]
-- f dbPool = runOrville

main :: IO ()
main = do
  dbPool  <- liftIO $ createPool (HDBC.connectPostgreSQL  "postgresql://postgres@localhost:5432" ) HDBC.disconnect 1 60 1
  liftIO $ putStrLn "pool created"
  -- why does this small transformer stack use IO in my `loggingOrvilleExSelect` function and how can I make a custom instance for AppDebugSql that prints out the sql in `runningQuery`?

  print @[String] =<< (flip runReaderT dbPool .  runApp $ do
                           -- withTransaction loggingOrvilleExSelect)
                          env <- getOrvilleEnv
                          runOrville (_ loggingOrvilleExSelect) env
                          )

-- experiments farther below if interested

















  -- 1.
  -- print @[String] =<< (flip runReaderT dbPool . runApp . runAppDebugSql $ do
  --                         -- _1 :: AppDebugSql (App (Pool HDBC.Connection) IO) [String]
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
