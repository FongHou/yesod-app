{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model where

import Data.Time.Clock (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH
import Model.Types
import Relude

share [mkPersist sqlSettings, mkMigrate "migrateAll"] --  $(persistFileWith lowerCaseSettings "config/models")
  [persistLowerCase|

User json
    email Email
    username Text
    password Password
    bio Text
    image Text
    createAt UTCTime
    updateAt UTCTime
    UniqueUserUsername username
    UniqueUserEmail email
    deriving Show Typeable

UserFollower
    user UserId
    follower UserId
    UniqueUserFollower user follower
    deriving Show Typeable

Article
    author UserId
    title Text
    slug Text
    description Text
    body Text
    createdAt UTCTime
    updatedAt UTCTime
    UniqueArticleSlug slug
    deriving Show Typeable

Tag
    name Text
    UniqueTagName name
    deriving Show Typeable

ArticleFavorite
    article ArticleId
    user UserId
    UniqueArticleFavorite article user
    deriving Show Typeable

ArticleTag
    article ArticleId
    tag TagId
    UniqueArticleTag article tag
    deriving Show Typeable

ArticleComment
    article ArticleId
    author UserId
    body Text
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show Typeable

|]

maybeUpdate ::
  PersistField typ =>
  EntityField v typ ->
  Maybe typ ->
  Maybe (Update v)
maybeUpdate label = fmap (label =.)
