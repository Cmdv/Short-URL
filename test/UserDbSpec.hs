{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module UserDbSpec where

import Api.ShortUrl
import Config (App, AppT (..), Config (..), Environment (..), makePool)
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Database.Persist.Postgresql (Entity (..), deleteWhere, get, insert, runSqlPool)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Types
import Init
import Logger (defaultLogEnv)
import Models
import Network.HTTP.Types.Header
import Servant
import Test.Hspec

runAppToIO :: Config -> App a -> IO a
runAppToIO config app = do
  result <- runExceptT $ runReaderT (runApp app) config
  case result of
    Left err -> throwIO err
    Right a -> return a

setupTeardown :: (Config -> IO a) -> IO ()
setupTeardown runTestsWith = do
  withConfig $ \config -> do
    env <- defaultLogEnv
    pool <- makePool Test env
    migrateDb pool
    runTestsWith
      config
        { configEnv = Test,
          configPool = pool
        }
    cleanDb pool
  where
    migrateDb :: ConnectionPool -> IO ()
    migrateDb pool = runSqlPool doMigrations pool
    cleanDb :: ConnectionPool -> IO ()
    cleanDb = deleteAllShortUrl
    deleteAllShortUrl :: ConnectionPool -> IO ()
    deleteAllShortUrl pool = do
      flip runSqlPool pool $ do deleteWhere ([] :: [Filter ShortUrl])

-- These tests assumes you have...
--   1. a Postgres `test` user
--   2. a `shorturltest` DB
spec :: Spec
spec =
  around setupTeardown $ do
    describe "ShortUrl" $ do
      it "returns expected added shortUrl" $ \config -> do
        let shortUrl = ShortUrl (T.pack "https://github.com/Cmdv") (T.pack "8n5yxdc")
        dbShortUrl <-
          runAppToIO config $ do
            newShortUrlID <- runDb $ insert shortUrl
            maybeShortUrl <- runDb $ get newShortUrlID
            case maybeShortUrl of
              Nothing -> return $ ShortUrl (T.pack "") (T.pack "")
              Just url -> return url
        dbShortUrl `shouldBe` shortUrl

      it "returns new shortUrl" $ \config -> do
        dbShortUrl <-
          runAppToIO config $ do
            createShortUrl (InputUrl testUrl)
        shortUrlShort dbShortUrl `shouldSatisfy` (not . T.null)

      it "should redirect given generated short url" $ \config -> do
        dbShortUrl <-
          runAppToIO config $ do
            createShortUrl (InputUrl testUrl)
        let genShortUrl = do
              shortUrlShort dbShortUrl
        Headers content hHeader <- runAppToIO config $ do
          checkShortUrl (T.unpack genShortUrl)
        content `shouldBe` NoContent
        getHeaders hHeader `shouldBe` [(hContentLocation, BS.pack "https://github.com/Cmdv")]

testUrl :: String
testUrl = "https://github.com/Cmdv"
