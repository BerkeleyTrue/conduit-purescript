module Server.Core.Services.Articles
  ( mkArticleService
  , ArticleService(..)
  , ArticleOutput(..)
  ) where

import Prelude

import Conduit.Yoga.Om (fromAffThrowLeftAsOm)
import Data.Date (Date)
import Data.List (List, catMaybes)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Server.Core.Domain.Article (Article, Tag)
import Server.Core.Domain.User (AuthorId, UserId)
import Server.Core.Ports.Ports (ArticleListInput, ArticleRepo(..))
import Server.Core.Services.User (PublicProfile, UserService)
import Slug (Slug)
import Yoga.Om (Om, expandCtx, fromAff, handleErrors)

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

newtype ArticleService = ArticleService
  { list :: { userId :: UserId, input :: ArticleListInput } -> Om {} () (List ArticleOutput)
  , getBySlug :: Slug -> Om {} (articleServiceError :: String) Article
  }

derive instance newtypeArticleService :: Newtype ArticleService _

listArticles
  :: forall ctx
   . ArticleRepo Aff
  -> UserService
  -> { userId :: UserId, input :: ArticleListInput }
  -> Om { | ctx } () (List ArticleOutput)
listArticles (ArticleRepo { list }) userService { userId, input } = do
  articles <- fromAff $ list input
  articlesOutput <- catMaybes <$> traverse mapArticle articles
  pure $ articlesOutput

  where
  getProfile :: AuthorId -> Om {} (userRepoErr :: String) PublicProfile
  getProfile = ((unwrap userService).getProfile) userId

  mapArticle :: Article -> Om { | ctx } () (Maybe ArticleOutput)
  mapArticle { slug, title, description, body, tagList, createdAt, updatedAt, authorId } = handleErrors { userRepoErr: pure <<< const Nothing } do
    profile <- expandCtx $ getProfile authorId
    pure $ Just
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

mkArticleService :: ArticleRepo Aff -> UserService -> ArticleService
mkArticleService articlesRepo@(ArticleRepo { getBySlug }) userService =
  ArticleService
    { getBySlug: getBySlug'
    , list: listArticles articlesRepo userService
    }
  where
  getBySlug' :: Slug -> Om {} (articleServiceError :: String) Article
  getBySlug' = fromAffThrowLeftAsOm (\err -> { articleServiceError: err }) <<< getBySlug
