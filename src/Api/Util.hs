{-# LANGUAGE NoImplicitPrelude #-}

module Api.Util where

import Control.Exception.Safe (Handler (..), IOException, MonadThrow, throwM)
import Data.Aeson
import qualified Data.Text as T
import Database.Persist (PersistException)
import Database.Persist.Sql (PersistentSqlException)
import Relude
import Servant hiding (Handler)
import Web.Forma

notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty text =
  if T.null text
    then throwError "This field cannot be empty."
    else return text

validate ::
  (MonadThrow m, ToJSON e, ToJSON b) =>
  FormParser names e m b ->
  Value ->
  m b
validate form body = do
  runForm form body >>= \case
    Succeeded a -> return a
    err@(ValidationFailed _) -> throwM $ err422 {errBody = encode err}
    ParsingFailed path msg ->
      throwM $
        err400 {errBody = encodeUtf8 $ key <> ": " <> msg}
      where
        key = maybe "" showFieldName path

dbErrors :: MonadThrow m => [Handler m a]
dbErrors =
  [ Handler $ \(e :: IOException) ->
      throwM err500 {errBody = show e <> ": no connection"},
    Handler $ \(e :: PersistException) -> throwM err500 {errBody = show e},
    Handler $ \(e :: PersistentSqlException) ->
      throwM err500 {errBody = show e}
  ]
