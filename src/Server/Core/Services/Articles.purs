module Server.Core.Services.Articles
  ( mkArticleService
  , ArticleService(..)
  , ArticleOutput(..)
  ) where

import Prelude

import Conduit.Data.MySlug (MySlug)
import Conduit.Data.UserId (AuthorId)
import Conduit.Data.Username (Username)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Server.Core.Domain.Article (Article, Tag)
import Server.Core.Ports.Ports (ArticleListInput, ArticleRepo(..))
import Server.Core.Services.User (PublicProfile, UserService)
import Yoga.Om (Om, expandCtx, expandErr, handleErrors)

type ArticleOutput =
  { slug :: MySlug
  , title :: String
  , description :: String
  , body :: String
  , tagList :: Array Tag
  , createdAt :: JSDate
  , updatedAt :: Maybe JSDate
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: PublicProfile
  }

newtype ArticleService = ArticleService
  { list :: { username :: (Maybe Username), input :: ArticleListInput } -> Om {} (articleRepoErr :: String) (Array ArticleOutput)
  , getBySlug :: MySlug -> Om {} (articleRepoErr :: String) Article
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

mkArticleService :: ArticleRepo -> UserService -> Om {} () ArticleService
mkArticleService articlesRepo@(ArticleRepo { getBySlug }) userService = pure $
  ArticleService
    { getBySlug: getBySlug
    , list: listArticles articlesRepo userService
    }
