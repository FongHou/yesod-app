{-# LANGUAGE NoImplicitPrelude #-}

module Api
  ( Config (..),
    app,
  )
where

import Import.NoFoundation
import Network.HTTP.Client (Manager)
import Servant
import Servant.API.Generic
import Servant.Server.Generic

type API = "id" :> ToServantApi TestR :<|> EmptyAPI

data TestR route
  = Routes
      { _get :: route :- Capture "id" Int :> Get '[JSON] String,
        _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
      }
  deriving (Generic)

data UserR route
  = UserR
      { getUser :: route :- Capture "userid" UserId :> Get '[JSON] User,
        putUser :: route :- Capture "userid" UserId :> ReqBody '[JSON] User :> Put '[JSON] User,
        registerUser :: route :- ReqBody '[JSON] User :> Post '[JSON] UserId
      }

data Config
  = Config
      { appConnPool :: ConnectionPool,
        appHttpManager :: Manager
      }
  deriving (Generic)

app :: Config -> Application
app config = serve api $ hoistServer api (usingReaderT config) server
  where
    api = Proxy :: Proxy API

type AppM = ReaderT Config Handler

server :: ServerT API AppM
server = genericServerT test :<|> emptyServer

test :: TestR (AsServerT AppM)
test =
  Routes
    { _get = return . show,
      _put = return . odd
    }
