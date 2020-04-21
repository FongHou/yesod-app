{-# LANGUAGE NoImplicitPrelude #-}

module Model.Types where

import Data.Aeson
import Data.CaseInsensitive
import qualified Data.CaseInsensitive as CI
import Database.Persist
import Database.Persist.Sql
import Relude
import qualified Text.Email.Validate as Email
import qualified Yesod.Auth.Util.PasswordStore as PS

newtype Password = Password Text deriving (Show, Eq, ToJSON, FromJSON)

mkPassword :: MonadIO m => Text -> m Password
mkPassword text = Password . decodeUtf8 <$> liftIO (PS.makePassword (encodeUtf8 text) 14)

verifyPassword :: Text -> Password -> Bool
verifyPassword rawPassword (Password password) =
  PS.verifyPassword (encodeUtf8 rawPassword) $ encodeUtf8 password

instance PersistField Password where
  toPersistValue (Password password) = PersistText password
  fromPersistValue (PersistText text) = Right $ Password text
  fromPersistValue x = Left $ "Password: expected PersistText, received: " <> show x

instance PersistFieldSql Password where
  sqlType _ = SqlString

newtype Email = Email (CI Text) deriving (Show, Eq)

mkEmail :: Text -> Maybe Email
mkEmail email =
  if Email.isValid $ encodeUtf8 email
    then Just $ Email $ CI.mk email
    else Nothing

instance PersistField Email where
  toPersistValue (Email email) = PersistText $ CI.original email
  fromPersistValue (PersistText text) = case mkEmail text of
    Just email -> Right email
    _ -> Left $ "Invalid email address: " <> text
  fromPersistValue x = Left $ "Invalid email address: expected PersistText, received: " <> show x

instance PersistFieldSql Email where
  sqlType _ = SqlString

instance ToJSON Email where
  toJSON (Email email) = String $ CI.original email

instance FromJSON Email where
  parseJSON = withText "Email" $ \text ->
    case mkEmail text of
      Just email -> return email
      _ -> fail $ toString $ "Invalid email address: " <> text
