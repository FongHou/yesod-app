{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api.User where

import Api.Config
import Api.Util
import Control.Exception.Safe
import Control.Monad.Except
import Data.Aeson
import Data.Time.Clock
import Database
import Database.Esqueleto hiding (Value)
import Model
import Model.Types
import Relude hiding (get)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Web.Forma

data Login = Login
  { loginEmail :: Email,
    loginPassword :: Text
  }
  deriving (Show, ToJSON, Generic)

type LoginFields = '["login", "email", "password"]

data Register = Register
  { registerUsername :: Text,
    registerEmail :: Email,
    registerPassword :: Text
  }
  deriving (Show, ToJSON, Generic)

type RegisterFields = '["register", "username", "email", "password"]

data UserAPI route = UserAPI
  { _get ::
      route
        :- Capture "userid" UserId
        :> Get '[JSON] User,
    _put ::
      route
        :- Capture "userid" UserId
        :> ReqBody '[JSON] Value
        :> Put '[JSON] User,
    _register ::
      route
        :- "register"
        :> ReqBody '[JSON] Value
        :> Post '[JSON] User,
    _login ::
      route
        :- "login"
        :> ReqBody '[JSON] Value
        :> Post '[JSON] User
  }
  deriving (Generic)

userApi :: UserAPI (AsServerT App)
userApi =
  UserAPI
    { _register = registerUser,
      _login = loginUser,
      _get = getUser,
      _put = putUser
    }

registerForm :: UseDB env m => FormParser RegisterFields Text m Register
registerForm =
  subParser
    #register
    ( Register
        <$> field #username (notEmpty >=> uniqueUsername)
        <*> field #email (notEmpty >=> validEmail >=> uniqueEmail)
        <*> field #password notEmpty
    )

registerUser :: Value -> App User
registerUser body = do
  Register {..} <- validate registerForm body
  pwdHash <- mkPassword registerPassword
  now <- liftIO getCurrentTime
  let user = User registerEmail registerUsername pwdHash "" "" now now
  void $ runDb (insert user)
  return user

loginForm :: Monad m => FormParser LoginFields Text m Login
loginForm =
  subParser
    #login
    ( Login
        <$> field #email (notEmpty >=> validEmail)
        <*> field #password notEmpty
    )

loginUser :: Value -> App User
loginUser login = do
  Login {..} <- validate loginForm login
  mUser <- runDb (getBy $ UniqueUserEmail loginEmail)
  case mUser of
    Just (Entity _ user@User {..})
      | validPwd -> return user
      where
        validPwd = verifyPassword loginPassword userPassword
    _ -> throw err401

getUser :: UserId -> App User
getUser userId = do
  mUser <- runDb (get userId)
  case mUser of
    Nothing -> throw err404
    Just user -> return user

putUser :: UserId -> Value -> App User
putUser _userId _body = throw err404 {errBody = "Not implemented yet."}

validEmail :: Monad m => Text -> ExceptT Text m Email
validEmail email =
  case mkEmail email of
    Just e -> return e
    _ -> throwError "Invalid email address."

uniqueEmail :: UseDB env m => Email -> ExceptT Text m Email
uniqueEmail email = do
  user <- runDb $ getBy $ UniqueUserEmail email
  case user of
    Nothing -> return email
    _ -> throwError "This email address is already being used."

uniqueUsername :: UseDB env m => Text -> ExceptT Text m Text
uniqueUsername username = do
  user <- runDb $ getBy $ UniqueUserUsername username
  case user of
    Nothing -> pure username
    _ -> throwError "This username is already being used."
