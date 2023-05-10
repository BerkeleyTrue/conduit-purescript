module Server.Core.Services.Articles
  ( mkArticleService
  , ArticleService(..)
  , ArticleOutput(..)
  ) where

import Prelude

import Conduit.Data.UserId (AuthorId)
import Conduit.Data.Username (Username)
import Data.Array (catMaybes)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Server.Core.Domain.Article (Article, Tag)
import Server.Core.Ports.Ports (ArticleListInput, ArticleRepo(..))
import Server.Core.Services.User (PublicProfile, UserService)
import Slug (Slug)
import Yoga.Om (Om, expandCtx, expandErr, handleErrors)

type ArticleOutput =
  { slug :: Slug
  , title :: String
  , description :: String
  , body :: String
  , tagList :: Array Tag
  , createdAt :: Date
  , updatedAt :: Maybe Date
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: PublicProfile
  }

newtype ArticleService = ArticleService
  { list :: { username :: (Maybe Username), input :: ArticleListInput } -> Om {} (articleRepoErr :: String) (Array ArticleOutput)
  , getBySlug :: Slug -> Om {} (articleRepoErr :: String) Article
  }

derive instance newtypeArticleService :: Newtype ArticleService _

listArticles
  :: ArticleRepo
  -> UserService
  -> { username :: (Maybe Username), input :: ArticleListInput }
  -> Om {} (articleRepoErr :: String) (Array ArticleOutput)
listArticles (ArticleRepo { list }) userService { username, input } = do
  articles <- expandErr $ list input
  catMaybes <$> traverse (expandErr <<< mapArticle) articles

  where
  getProfile' = (unwrap userService).getProfile

  getProfile :: AuthorId -> Om {} (userRepoErr :: String) PublicProfile
  getProfile = flip getProfile' username <<< Left

  mapArticle :: Article -> Om {} () (Maybe ArticleOutput)
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

mkArticleService :: ArticleRepo -> UserService -> ArticleService
mkArticleService articlesRepo@(ArticleRepo { getBySlug }) userService =
  ArticleService
    { getBySlug: getBySlug
    , list: listArticles articlesRepo userService
    }
