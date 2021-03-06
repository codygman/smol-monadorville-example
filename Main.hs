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
  )

newtype AppDebugSql m a =
  AppDebugSql { runAppDebugSql :: m a }
  deriving ( Applicative
           , Monad
           , Functor
           , MonadIO
           , MonadBase b
           , MonadBaseControl b
           , MonadFail
           , MonadThrow
           )

-- instance 1
instance MonadOrville HDBC.Connection (App (Pool HDBC.Connection) IO) where
  getOrvilleEnv = newOrvilleEnv <$> App ask
  runningQuery _ _ query = do
    liftIO $ putStrLn "instance 1"
    query
  localOrvilleEnv f = App . local id . runApp

-- instance 2
-- NOTE my previous WRONG understanding was that `AppDebugSql` should act as the base monad and that the instead head below would be selected in `main :: IO` with `void . flip runReaderT dbPool .  runApp . runAppDebugSql $ loggingOrvilleExSelect`
-- instance MonadOrville HDBC.Connection (App (Pool HDBC.Connection) (AppDebugSql IO)) where

-- instance 3
instance MonadOrville HDBC.Connection (AppDebugSql (App (Pool HDBC.Connection) IO)) where
  getOrvilleEnv = AppDebugSql $ do
    getOrvilleEnv
  runningQuery _ _ query = do
    liftIO $ putStrLn "instance 3"
    query
  localOrvilleEnv f = AppDebugSql . App . local id . runApp . runAppDebugSql

main :: IO ()
main = do
  dbPool  <- liftIO $ createPool (HDBC.connectPostgreSQL  "postgresql://postgres@localhost:5432" ) HDBC.disconnect 1 60 1

  putStrLn "1. non-logging"
  void . flip runReaderT dbPool .  runApp  $
    orvilleExSelect
  putStrLn ""

  putStrLn "2. logging"
  void . flip runReaderT dbPool .  runApp . runAppDebugSql $
    loggingOrvilleExSelect
  putStrLn ""

  -- This one is surprising to me
  putStrLn "3. think I can \"wrap\" App (Pool ...) IO [String] to change typeclass selected and get logging behavior. I expected `instance 3` but it used `instance 1`. Why?"
  void . flip runReaderT dbPool .  runApp  . runAppDebugSql . AppDebugSql $ do
    orvilleExSelect
  putStrLn ""

  putStrLn "4. inline selectSql call, use runApp to select non-logging MonadOrville instance"
  void . flip runReaderT dbPool .  runApp $ do
    selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (col @Text.Text "aggnumdirectargs")
  putStrLn ""

  putStrLn "5. inline selectSql call, use runApp to select logging MonadOrville instance"
  void . flip runReaderT dbPool .  runApp . runAppDebugSql $ do
    selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (col @Text.Text "aggnumdirectargs")
  putStrLn ""

loggingOrvilleExSelect :: AppDebugSql (App (Pool HDBC.Connection) IO) [String]
loggingOrvilleExSelect = do
    selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (col @Text.Text "aggnumdirectargs")

orvilleExSelect :: App (Pool HDBC.Connection) IO [String]
orvilleExSelect = do
    selectSql @String
      "select aggnumdirectargs from pg_catalog.pg_aggregate pa limit 2;"
      []
      (col @Text.Text "aggnumdirectargs")

-- sample output of this revision:
{-
1. non-logging
instance 1

2. logging
instance 3

3. think I can "wrap" App (Pool ...) IO [String] to change typeclass selected and get logging behavior. I expected `instance 3` but it used the default MonadOrville instance

4. inline selectSql call, use runApp to select non-logging MonadOrville instance
instance 1

5. inline selectSql call, use runApp to select logging MonadOrville instance
instance 3


...done
-}
