module Server.Core.Services.Articles
  ( mkArticleService
  , ArticleService(..)
  , ArticleOutput(..)
  , ArticleUpdateInput(..)
  , ArticleServiceErrs
  ) where

import Prelude

import Conduit.Data.ArticleId (ArticleId)
import Conduit.Data.MySlug (MySlug)
import Conduit.Data.UserId (AuthorId)
import Conduit.Data.Username (Username)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Data.Traversable (traverse)
import Server.Core.Domain.Article (Article, Tag)
import Server.Core.Ports.Ports (ArticleListInput, ArticleRepo(..))
import Server.Core.Services.User (UserService(..), UserServiceErrs, PublicProfile)
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

type ArticleUpdateInput =
  { title :: Maybe String
  , description :: Maybe String
  , body :: Maybe String
  }

type ArticleServiceErrs r = (UserServiceErrs (articleRepoErr :: String | r))

newtype ArticleService = ArticleService
  { list :: { username :: (Maybe Username), input :: ArticleListInput } -> Om {} (ArticleServiceErrs ()) (Array ArticleOutput)
  , getBySlug :: MySlug -> (Maybe Username) -> Om {} (ArticleServiceErrs ()) ArticleOutput
  , getIdFromSlug :: MySlug -> Om {} (ArticleServiceErrs ()) ArticleId
  , update :: MySlug -> Username -> ArticleUpdateInput -> Om {} (ArticleServiceErrs ()) ArticleOutput
  , delete :: MySlug -> Om {} (ArticleServiceErrs ()) Unit
  , favorite :: MySlug -> Username -> Om {} (ArticleServiceErrs ()) ArticleOutput
  , unfavorite :: MySlug -> Username -> Om {} (ArticleServiceErrs ()) ArticleOutput
  }

derive instance newtypeArticleService :: Newtype ArticleService _

mkArticleOutput :: Article -> PublicProfile -> ArticleOutput
mkArticleOutput { slug, title, description, body, tagList, createdAt, updatedAt } profile =
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

mkList
  :: ArticleRepo
  -> UserService
  -> { username :: (Maybe Username), input :: ArticleListInput }
  -> Om {} (ArticleServiceErrs ()) (Array ArticleOutput)
mkList (ArticleRepo { list }) userService { username, input } = do
  articles <- expandErr $ list input
  catMaybes <$> traverse (expandErr <<< mapArticle) articles

  where
  getProfile' = (unwrap userService).getProfile

  getProfile :: AuthorId -> Om {} (userRepoErr :: String) PublicProfile
  getProfile = flip getProfile' username <<< Left

  mapArticle :: Article -> Om {} () (Maybe ArticleOutput)
  mapArticle article = handleErrors { userRepoErr: pure <<< const Nothing } do
    profile <- expandCtx $ getProfile article.authorId
    pure $ Just $ mkArticleOutput article profile

mkGetBySlug :: ArticleRepo -> UserService -> MySlug -> (Maybe Username) -> Om {} (ArticleServiceErrs ()) ArticleOutput
mkGetBySlug (ArticleRepo { getBySlug }) (UserService { getProfile }) slug username = do
  article <- expandErr $ getBySlug slug
  profile <- expandErr $ getProfile (Left article.authorId) username
  pure $ mkArticleOutput article profile

mkUpdate :: ArticleRepo -> UserService -> MySlug -> Username -> ArticleUpdateInput -> Om {} (ArticleServiceErrs ()) ArticleOutput
mkUpdate (ArticleRepo { update }) (UserService { getProfile }) slug username input = do
  article <- expandErr $ update slug
    ( \article -> article
        { title = fromMaybe article.title input.title
        , description = fromMaybe article.description input.description
        , body = fromMaybe article.body input.body
        }
    )

  profile <- expandErr $ getProfile (Left article.authorId) (Just username)
  pure $ mkArticleOutput article profile

mkDelete :: ArticleRepo -> MySlug -> Om {} (ArticleServiceErrs ()) Unit
mkDelete (ArticleRepo { delete }) slug = do
  expandErr $ delete slug

mkFavorite :: ArticleRepo -> UserService -> MySlug -> Username -> Om {} (ArticleServiceErrs ()) ArticleOutput
mkFavorite (ArticleRepo { update }) (UserService { getProfile, getIdFromUsername }) slug username = do
  userId <- expandErr $ getIdFromUsername username
  article <- expandErr $ update slug
    ( \article -> article
        { favoritedBy = Set.insert userId article.favoritedBy
        }
    )

  profile <- expandErr $ getProfile (Left article.authorId) (Just username)
  pure $ mkArticleOutput article profile

mkUnfavorite :: ArticleRepo -> UserService -> MySlug -> Username -> Om {} (ArticleServiceErrs ()) ArticleOutput
mkUnfavorite (ArticleRepo { update }) (UserService { getProfile, getIdFromUsername }) slug username = do
  userId <- expandErr $ getIdFromUsername username
  article <- expandErr $ update slug
    ( \article -> article
        { favoritedBy = Set.delete userId article.favoritedBy
        }
    )

  profile <- expandErr $ getProfile (Left article.authorId) (Just username)
  pure $ mkArticleOutput article profile

mkArticleService :: ArticleRepo -> UserService -> ArticleService
mkArticleService articlesRepo@(ArticleRepo { getBySlug }) userService =
  ArticleService
    { getBySlug: mkGetBySlug articlesRepo userService
    , getIdFromSlug: expandErr <<< liftM1 _.articleId <<< getBySlug
    , list: mkList articlesRepo userService
    , update: mkUpdate articlesRepo userService
    , delete: mkDelete articlesRepo
    , favorite: mkFavorite articlesRepo userService
    , unfavorite: mkUnfavorite articlesRepo userService
    }
