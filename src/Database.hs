module Database where

import Data.Generics.Product.Typed
import Database.Persist.Sql
import Optics
import Relude

runDb :: (HasType ConnectionPool env, MonadReader env m, MonadIO m) => SqlPersistT IO a -> m a
runDb query = do
  pool <- asks $ view $ typed @ConnectionPool
  liftIO $ runSqlPool query pool
