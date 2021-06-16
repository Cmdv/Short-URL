{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Init where

import Control.Concurrent (killThread)
import Control.Exception.Safe
import qualified Control.Monad.Metrics as M
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Database.Persist.Postgresql (runSqlPool)
import qualified Katip
import Lens.Micro ((^.))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Metrics (metrics, registerWaiMetrics)
import Safe (readMay)
import Say
import System.Environment (lookupEnv)
import System.Remote.Monitoring (forkServer, serverMetricStore, serverThreadId)

import Api (app)
import Config (Config (..), Environment (..), makePool, setLogger)
import Helper (tshow)
import Logger (defaultLogEnv)
import Models (doMigrations)

-- | Creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runAppDevel :: IO ()
runAppDevel = do
  say "in runAppDevel"
  withConfig $ \config -> do
    say "acquired config"
    cfg <-
      initialize config
        `finally` say "exited: initialize config"
    say "post-initialize"
    run (configPort config) cfg
      `finally` say "server is closed"

-- | Initializes the WAI 'Application'
initialize :: Config -> IO Application
initialize cfg = do
  say "initialize"
  waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
  say "wai metrics registered"
  let logger = setLogger (configEnv cfg)
  say "run migrations"
  bracket
    (say "starting to run migrations")
    (\_ -> say "migrations complete")
    $ \_ -> do
      say "actually running migrations"
      runSqlPool doMigrations (configPool cfg) `catch` \(SomeException e) -> do
        say $
          mconcat
            [ "exception in doMigrations, type: ",
              tshow (typeOf e),
              ", shown: ",
              tshow e
            ]
        throwIO e
      say "okay all done"

  say "making app"
  pure . logger . metrics waiMetrics . app $ cfg

-- | Build up the configurations
withConfig :: (Config -> IO a) -> IO a
withConfig action = do
  say "acquireConfig"
  port <- lookupSetting "PORT" 8081
  say $ "on port:" <> tshow port
  env <- lookupSetting "ENV" Development
  say $ "on env: " <> tshow env
  bracket defaultLogEnv (\x -> say "closing katip scribes" >> Katip.closeScribes x) $ \logEnv -> do
    say "got log env"
    !pool <- makePool env logEnv `onException` say "exception in makePool"
    say "got pool "
    bracket (forkServer "localhost" 8082) (\x -> say "closing ekg" >> do killThread $ serverThreadId x) $ \ekgServer -> do
      say "forked ekg server"
      let store = serverMetricStore ekgServer
      metr <- M.initializeWith store
      say "got metrics"
      action
        Config
          { configPool = pool,
            configEnv = env,
            configMetrics = metr,
            configLogEnv = logEnv,
            configPort = port,
            configEkgServer = serverThreadId ekgServer
          }

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  Katip.closeScribes (configLogEnv cfg)
  Pool.destroyAllResources (configPool cfg)
  killThread (configEkgServer cfg)
  pure ()

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      return def
    Just str ->
      maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $
        mconcat
          [ "Failed to read [[",
            str,
            "]] for environment variable ",
            env
          ]
