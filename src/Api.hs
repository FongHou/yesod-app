{-# LANGUAGE TypeOperators, DataKinds, DeriveGeneric #-}

module Api (API, api, app, module Servant) where

import Servant hiding (Handler(..))
import Servant.API.Generic
import Servant.Server.Generic

type API = "id" :> ToServantApi Routes :<|> EmptyAPI

data Routes route = Routes
    { _get :: route :- Capture "id" Int :> Get '[JSON] String
    , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
    }
  deriving (Generic)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = genericServerT routes :<|> emptyServer

routes :: Routes AsServer
routes = Routes
    { _get = return . show
    , _put = return . odd
    }

