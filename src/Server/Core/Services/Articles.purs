module Server.Core.Services.Articles
  ( mkArticleService
  , ArticleService(..)
  , ArticleOutput(..)
  ) where

import Prelude

import Data.Date (Date)
import Data.Either (Either(..))
import Data.List (List, catMaybes)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Server.Core.Domain.Article (Article, Tag)
import Server.Core.Domain.User (UserId, AuthorId)
import Server.Core.Ports.Ports (ArticleListInput, ArticleRepo(..))
import Server.Core.Services.User (PublicProfile, UserService)
import Slug (Slug)
import Yoga.Om (Om, expandCtx, expandErr, handleErrors)

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
  { list :: { userId :: (Maybe UserId), input :: ArticleListInput } -> Om {} (articleRepoErr :: String) (List ArticleOutput)
  , getBySlug :: Slug -> Om {} (articleRepoErr :: String) Article
  }

derive instance newtypeArticleService :: Newtype ArticleService _

listArticles
  :: ArticleRepo
  -> UserService
  -> { userId :: (Maybe UserId), input :: ArticleListInput }
  -> Om {} (articleRepoErr :: String) (List ArticleOutput)
listArticles (ArticleRepo { list }) userService { userId, input } = do
  articles <- expandErr $ list input
  articlesOutput <- catMaybes <$> traverse (expandErr <<< mapArticle) articles
  pure $ articlesOutput

  where
  getProfile' = (unwrap userService).getProfile

  getProfile :: AuthorId -> Om {} (userRepoErr :: String) PublicProfile
  getProfile = flip getProfile' userId <<< Left

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
