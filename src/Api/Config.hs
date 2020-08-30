module Api.Config
  ( App (..),
    Config (..),
  )
where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Import.NoFoundation
import Network.HTTP.Client (Manager)

data Config = Config
  { appConnPool :: ConnectionPool,
    appHttpManager :: Manager
  }
  deriving (Generic)

newtype App a = App {runApp :: ReaderT Config IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader Config
    )
