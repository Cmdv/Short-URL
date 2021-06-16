{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import Control.Concurrent (ThreadId)
import Control.Exception.Safe (throwIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger (..))
import Control.Monad.Metrics (Metrics, MonadMetrics, getMetrics)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Postgresql
  ( ConnectionPool,
    ConnectionString,
    createPostgresqlPool,
  )
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant (ServerError)
import System.Environment (lookupEnv)

import Logger

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServerError m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
  { configPool :: ConnectionPool,
    configEnv :: Environment,
    configMetrics :: Metrics,
    configEkgServer :: ThreadId,
    configLogEnv :: LogEnv,
    configPort :: Port
  }

instance Monad m => MonadMetrics (AppT m) where
  getMetrics = asks Config.configMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
  getLogEnv = asks configLogEnv
  localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
  monadLoggerLog = adapt logMsg

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

-- | Web request logger (currently unimplemented and unused). For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
  -- todo: log proper request data
  logMsg "web" InfoS "todo: received some request"
  liftIO $ app req respond

-- | This function creates a 'ConnectionPool' for the given environment.
makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Test env =
  runKatipT env (createPostgresqlPool (connStr "test") (envPool Test))
makePool Development env =
  runKatipT env $ createPostgresqlPool (connStr "") (envPool Development)
makePool Production env = do
  pool <- runMaybeT $ do
    let keys =
          [ "host=",
            "port=",
            "user=",
            "password=",
            "dbname="
          ]
        envs =
          [ "PGHOST",
            "PGPORT",
            "PGUSER",
            "PGPASS",
            "PGDATABASE"
          ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
    lift $ runKatipT env $ createPostgresqlPool prodStr (envPool Production)
  case pool of
    Nothing -> throwIO (userError "Database Configuration not present in environment.")
    Just a -> return a

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

-- | These creds would not be stored here and rather passed as a Env Varialble
connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=shorturl" <> sfx <> " user=test password=test port=5432"
