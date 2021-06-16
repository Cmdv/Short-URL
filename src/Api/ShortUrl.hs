{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Api.ShortUrl where

import Config (AppT (..))
import Control.Monad.Except (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugNS)
import Control.Monad.Metrics (increment)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (pack)
import Database.Persist.Postgresql
  ( Entity (..),
    get,
    insert,
    selectFirst,
    (==.),
  )
import GHC.Generics (Generic)
import GHC.TypeLits
import Servant

import Helper (shortUrlGen)
import Models (ShortUrl (..), runDb)
import qualified Models as Md

type ShortUrlAPI =
  "shorten" :> ReqBody '[JSON] InputUrl :> Post '[JSON] ShortUrl
    :<|> Capture "url" String :> PostRedirect 301 String

shortUrlApi :: Proxy ShortUrlAPI
shortUrlApi = Proxy

data InputUrl =
  InputUrl
  { url :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON InputUrl
instance FromJSON InputUrl

-- | The server that runs the ShortUrlAPI
shortUrlServer :: MonadIO m => ServerT ShortUrlAPI (AppT m)
shortUrlServer = createShortUrl :<|> checkShortUrl

-- | Creates a short Url in the database.
createShortUrl :: MonadIO m => InputUrl -> AppT m ShortUrl
createShortUrl p = do
  increment "createShortUrl"
  logDebugNS "web" "creating a shortURL"
  shortg <- pure =<< liftIO shortUrlGen
  newShortUrlID <- runDb (insert (ShortUrl (pack $ url p) (pack shortg)))
  maybeShortUrl <- runDb (get newShortUrlID)
  case maybeShortUrl of
    Nothing -> throwError err404
    Just url -> return url

-- | a custom Verb to deal with redirecting
type PostRedirect (code :: Nat) loc =
  Verb 'GET code '[JSON] (Headers '[Header "Content-Location" loc] NoContent)

-- | Returns a 301 redirection or throws a 404 error
checkShortUrl ::
  forall loc (m :: * -> *).
  (ToHttpApiData loc, FromHttpApiData loc, MonadIO m) =>
  loc ->
  AppT m (Headers '[Header "Content-Location" loc] NoContent)
checkShortUrl param = do
  increment "checkShortUrl"
  logDebugNS "web" "checkShortUrl"
  maybeUrl <- runDb (selectFirst [Md.ShortUrlShort ==. toQueryParam param] [])
  case maybeUrl of
    Nothing ->
      throwError err404
    Just (Entity _ url) ->
      case parseQueryParam $ shortUrlLong url of
        Left _ -> throwError err404
        Right u -> return $ addHeader u NoContent
