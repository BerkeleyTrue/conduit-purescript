module Server.Core.Domain.Article where

import Data.Bounded (class Ord)
import Data.Date (Date)
import Data.Eq (class Eq)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Show (class Show, show)
import Data.UUID (UUID)
import Server.Core.Domain.User (UserId)
import Slug (Slug)

newtype ArticleId = ArticleId UUID

instance showArticleId :: Show ArticleId where
  show (ArticleId uuid) = show uuid

derive instance eqArticleId :: Eq ArticleId
derive instance ordArticleId :: Ord ArticleId

data Article = Article
  { articleId :: ArticleId
  , slug :: Slug
  , title :: String
  , description :: String
  , authorId :: UserId
  , body :: String
  , tagList :: List String
  , favoritesCount :: Int
  , createdAt :: Date
  , updatedAt :: Maybe Date
  }
