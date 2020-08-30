{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR= do
  (Entity _ user) <- requireAuth
  let username = userUsername user
  defaultLayout $ do
    setTitle "User Profile"
    $(widgetFile "profile")
