{-# LANGUAGE NoImplicitPrelude #-}

module Api
  ( module Api.Config,
    app,
  )
where

import Api.Config
import Api.User
import Control.Exception.Safe
import Relude
import Servant
import Servant.API.Generic
import Servant.Server.Generic

type API =
  EmptyAPI
    :<|> "user" :> ToServantApi UserAPI

app :: Config -> Application
app config = serve api $ hoistServer api (app' config) server
  where
    api = Proxy :: Proxy API
    app' cfg = Servant.Handler . ExceptT . try . usingReaderT cfg . runApp

server :: ServerT API App
server =
  emptyServer
    :<|> genericServerT userApi
