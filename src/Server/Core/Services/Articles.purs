module Server.Core.Services.Articles
  ( mkArticleService
  , ArticleService(..)
  , ArticleOutput(..)
  ) where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.List (List, catMaybes)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect)
import Server.Core.Domain.Article (Article, Tag)
import Server.Core.Domain.User (AuthorId, UserId)
import Server.Core.Ports.Ports (ArticleListInput, ArticleRepo(..))
import Server.Core.Services.User (PublicProfile, UserService)
import Slug (Slug)

type ArticleOutput =
  { slug :: Slug
  , title :: String
  , description :: String
  , body :: String
  , tagList :: List Tag
  , createdAt :: Date
  , updatedAt :: Maybe Date
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: PublicProfile
  }

newtype ArticleService m = ArticleService
  { list :: { userId :: UserId, input :: ArticleListInput } -> m (Either String (List ArticleOutput))
  , getBySlug :: Slug -> m (Either String Article)
  }

derive instance newtypeArticleService :: Newtype (ArticleService m) _

listArticles
  :: forall m ctx
   . MonadEffect m
  => ArticleRepo m
  -> UserService ctx
  -> { userId :: UserId, input :: ArticleListInput }
  -> m (Either String (List ArticleOutput))
listArticles (ArticleRepo { list }) userService { userId, input } = runExceptT do
  articles <- lift $ list input
  articlesOutput <- lift $ (catMaybes <$> traverse mapArticle articles)
  pure $ articlesOutput

  where
  getProfile = ((unwrap userService).getProfile) userId

  getProfile' :: AuthorId -> ExceptT String m PublicProfile
  getProfile' = ExceptT <<< getProfile

  mapArticle :: Article -> m (Maybe ArticleOutput)
  mapArticle { slug, title, description, body, tagList, createdAt, updatedAt, authorId } = do
    profile' <- runExceptT $ getProfile' authorId
    pure $ case profile' of
      Left _ -> Nothing
      Right profile ->
        Just
          { slug
          , title
          , description
          , body
          , tagList
          , createdAt
          , updatedAt
          , favorited: false
          , favoritesCount: 0
          , author: profile
          }

mkArticleService :: forall m. MonadEffect m => ArticleRepo m -> UserService m -> ArticleService m
mkArticleService articlesRepo@(ArticleRepo { getBySlug }) userService =
  ArticleService
    { getBySlug
    , list: listArticles articlesRepo userService
    }
