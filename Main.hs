module Main where

import qualified Database.Orville              as Orville
import qualified Database.Orville.Raw          as Orville
import qualified Control.Monad.Reader          as Reader
import qualified Control.Monad.Trans.Control   as Control
import qualified Control.Monad.Trans.Resource  as Resource
import qualified Control.Monad.Base            as Base
import qualified Database.HDBC.PostgreSQL      as Postgres

data EnvironmentWith context = EnvironmentWith
  { environmentContext :: context
  }

newtype App context m a = UrzaT (Reader.ReaderT (EnvironmentWith context) m a) deriving
  ( Applicative
  , Functor
  , Monad
  , Base.MonadBase b
  , Control.MonadBaseControl b
  , Reader.MonadIO
  )

newtype LoggingOrville c m a = LoggingOrville { runLoggingOrville :: Orville.OrvilleT c m a } deriving (Applicative, Monad, Functor, MonadFail)

-- https://taylor.fausak.me/orville/Database-Orville-Core.html#t:MonadOrville
instance (Monad m, Reader.MonadIO (LoggingOrville Postgres.Connection m), Control.MonadBaseControl IO (LoggingOrville Postgres.Connection m)) => Orville.MonadOrville Postgres.Connection (LoggingOrville Postgres.Connection m) where

{-
Main.hs:31:17: error:
    • Found hole: _ :: LoggingOrville conn0 m0 [()] -> App String IO a0
      Where: ‘conn0’ is an ambiguous type variable
             ‘m0’ is an ambiguous type variable
             ‘a0’ is an ambiguous type variable
    • In the first argument of ‘(.)’, namely ‘_’
      In the second argument of ‘(.)’, namely
        ‘_ . Orville.withTransaction @Postgres.Connection . LoggingOrville’
      In the expression:
        Reader.void
          . _ . Orville.withTransaction @Postgres.Connection . LoggingOrville
    • Relevant bindings include
        runLoggedQuery :: App String IO () (bound at Main.hs:30:1)
   |
31 |   Reader.void . _ . Orville.withTransaction @Postgres.Connection .
LoggingOrville $ Orville.selectSql
-}
runLoggedQuery :: App String IO ()
runLoggedQuery = do
  Reader.void . _whatGoesHere . Orville.withTransaction @Postgres.Connection . LoggingOrville $ Orville.selectSql
    ""
    []
    (undefined :: Orville.FromSql ())


main :: IO ()
main = putStrLn "Hello, Haskell!"
