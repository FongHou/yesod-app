{-# LANGUAGE NoImplicitPrelude #-}

module Database where

-- import qualified Data.Aeson as JSON

import Control.Exception.Safe
import Data.Generics.Product.Typed
import Database.Esqueleto
import Model
import Optics hiding ((^.))
import Relude hiding (isNothing, on)

type UseDB env m =
  ( HasType ConnectionPool env,
    MonadReader env m,
    MonadCatch m,
    MonadIO m
  )

runDb :: UseDB env m => SqlPersistT IO a -> m a
runDb query = do
  pool <- asks . view $ typed @ConnectionPool
  liftIO $ runSqlPool query pool

data ArticleData = ArticleData
  { article :: Entity Article,
    author :: Entity User,
    tags :: [Value Text],
    following :: Value Bool,
    favorited :: Value Bool,
    favoritesCount :: Value Int
  }

type ArticleClause =
  -- | article
  SqlExpr (Entity Article) ->
  -- | article's author
  SqlExpr (Entity User) ->
  -- | is current user following author
  SqlExpr (Value Bool) ->
  SqlQuery ()

getArticle :: UseDB env m => Maybe UserId -> ArticleId -> m (Maybe ArticleData)
getArticle mCurrentUserId articleId = do
  articles <- getArticles mCurrentUserId $ \article _ _ ->
    where_ $ article ^. ArticleId ==. val articleId
  return $ listToMaybe articles

getArticlesCount :: UseDB env m => Maybe UserId -> ArticleClause -> m [Value Int]
getArticlesCount mCurrentUserId extraClause = runDb $
  select $
    from $
      \(article `InnerJoin` author `LeftOuterJoin` mFollower) -> do
        let following = not_ $ isNothing $ mFollower ?. UserFollowerId
        on $
          mFollower ?. UserFollowerUser ==. just (author ^. UserId)
            &&. mFollower ?. UserFollowerFollower ==. val mCurrentUserId
        on $ article ^. ArticleAuthor ==. author ^. UserId
        orderBy [desc $ article ^. ArticleCreatedAt]
        extraClause article author following
        return countRows

getArticles :: UseDB env m => Maybe UserId -> ArticleClause -> m [ArticleData]
getArticles mCurrentUserId extraClause = do
  let articalFavorites article = from $ \favorite -> do
        where_ $ favorite ^. ArticleFavoriteArticle ==. article ^. ArticleId
        return (countRows :: SqlExpr (Value Int))
      addTags
        ( article@(Entity articleId _),
          author,
          following,
          favorited,
          favoritesCount
          ) = do
          tags <- getArticleTags articleId
          return $ ArticleData {..}
  articles <-
    runDb $
      select $
        from $
          \( article `InnerJoin` author
               `LeftOuterJoin` mFollower
               `LeftOuterJoin` mFavourite
             ) -> do
              let following = not_ $ isNothing $ mFollower ?. UserFollowerId
                  favorited = not_ $ isNothing $ mFavourite ?. ArticleFavoriteId
                  favoritesCount = subSelectCount $ articalFavorites article
              on $ mFavourite ?. ArticleFavoriteUser ==. val mCurrentUserId
              on $
                mFollower ?. UserFollowerUser ==. just (author ^. UserId)
                  &&. mFollower
                  ?. UserFollowerFollower
                  ==. val mCurrentUserId
              on $ article ^. ArticleAuthor ==. author ^. UserId
              orderBy [desc $ article ^. ArticleCreatedAt]
              extraClause article author following
              return (article, author, following, favorited, favoritesCount)
  mapM addTags articles

getArticleTags :: UseDB env m => ArticleId -> m [Value Text]
getArticleTags articleId =
  runDb $
    select $
      from $ \(articleTag `InnerJoin` tag) -> do
        where_ $ articleTag ^. ArticleTagArticle ==. val articleId
        where_ $ tag ^. TagId ==. articleTag ^. ArticleTagTag
        return $ tag ^. TagName

-- | Get user's article feed by tag, author or favorited by with pagination.
getUserArticleFeed ::
  UseDB env m =>
  Maybe UserId ->
  Page ->
  m ([ArticleData], Int)
getUserArticleFeed userId page = do
  let clause _ _ following = where_ following
  paginateArticles
    (getArticles userId)
    (getArticlesCount userId)
    page
    clause

newtype CommentData
  = CommentData
      ( Entity ArticleComment,
        Entity User,
        Value Bool
      )

type CommentClause =
  -- | comment
  SqlExpr (Entity ArticleComment) ->
  -- | article this comment belongs to
  SqlExpr (Entity Article) ->
  SqlQuery ()

-- | Get article's comment by ID.
getComment ::
  UseDB env m =>
  Maybe UserId ->
  ArticleCommentId ->
  m (Maybe CommentData)
getComment mCurrentUserId commentId = do
  comments <- getComments mCurrentUserId $ \comment _ ->
    where_ $ comment ^. ArticleCommentId ==. val commentId
  return $ listToMaybe comments

-- | Get comments for a given clause.
getComments :: UseDB env m => Maybe UserId -> CommentClause -> m [CommentData]
getComments mCurrentUserId extraClause = do
  comments <-
    runDb $
      select $
        from $
          \(comment `InnerJoin` article `InnerJoin` author `LeftOuterJoin` mFollower) -> do
            let following = not_ $ isNothing $ mFollower ?. UserFollowerId
            on $
              mFollower ?. UserFollowerUser ==. just (author ^. UserId)
                &&. mFollower ?. UserFollowerFollower ==. val mCurrentUserId
            on $ author ^. UserId ==. comment ^. ArticleCommentAuthor
            on $ article ^. ArticleId ==. comment ^. ArticleCommentArticle
            extraClause comment article
            return (comment, author, following)
  return $ CommentData <$> comments

-- | Get article's comments by article's slug.
getCommentsByArticleSlug ::
  UseDB env m =>
  Maybe UserId ->
  Text ->
  m [CommentData]
getCommentsByArticleSlug mCurrentUserId slug =
  getComments mCurrentUserId $ \_ article ->
    where_ $ article ^. ArticleSlug ==. val slug

-- | Pagination helper
data Page = Page
  { pageLimit :: !Int64,
    pageOffset :: !Int64
  }

paginate :: Page -> SqlQuery ()
paginate Page {..} = do
  limit pageLimit
  offset pageOffset

-- | Paginate articles produced by a given query.
paginateArticles ::
  Monad m =>
  -- | articles query
  (ArticleClause -> m [article]) ->
  -- | count query
  (ArticleClause -> m [Value Int]) ->
  -- | page settings
  Page ->
  -- | clause used in both queries
  ArticleClause ->
  m ([article], Int)
paginateArticles query countQuery page clause = do
  aCount <- countQuery clause
  articles <-
    query $ \article author following -> do
      clause article author following
      paginate page
  case aCount of
    [Value articleCount] ->
      return (articles, articleCount)
    _ ->
      return (articles, length articles)
