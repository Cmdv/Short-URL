{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DevelMain where

import Prelude

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Typeable
import Foreign.Store (Store (..), lookupStore, readStore, storeAction, withStore)
import GHC.Word (Word32)
import Say
import System.IO

import Helper (tshow)
import Init (runAppDevel)

-- | Start or restart the server.
update :: IO ()
update = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing -> do
      say "no server running"
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    Just tidStore -> do
      say "restarting app..."
      restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
      say $ "killing thread: " <> tshow tid
      killThread tid
      say "taking mvar"
      withStore doneStore takeMVar
      readStore doneStore >>= start
    start :: MVar () -> IO ThreadId
    start done =
      myThreadId
        <* ( do
               say "in forkFinally"
               runAppDevel `catch` \(SomeException e) -> do
                 say "!!! exception in runAppDevel !!!"
                 say $ "X    exception type: " <> tshow (typeOf e)
                 say $ "X    exception     : " <> tshow e
               say "runAppDevel terminated"
           )
        `catch` ( \(SomeException err) -> do
                    say "finally action"
                    hFlush stdout
                    hFlush stderr
                    putMVar done ()
                    say $ "Got Exception: " <> tshow err
                    throwIO err
                )
        `finally` ( do
                      say "finally action"
                      hFlush stdout
                      hFlush stderr
                      putMVar done ()
                  )

-- | kill the server
shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> say "no app running"
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      say "App is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref
