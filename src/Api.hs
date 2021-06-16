{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Control.Monad.Reader (runReaderT)
import Servant

import Api.ShortUrl (ShortUrlAPI, shortUrlApi, shortUrlServer)
import Config (AppT (..), Config (..))

shortUrlApp :: Config -> Application
shortUrlApp cfg = serve shortUrlApi (appToServer cfg)

appToServer :: Config -> Server ShortUrlAPI
appToServer cfg = hoistServer shortUrlApi (convertApp cfg) shortUrlServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

appApi :: Proxy ShortUrlAPI
appApi = Proxy

app :: Config -> Application
app cfg =
  serve appApi (appToServer cfg)
