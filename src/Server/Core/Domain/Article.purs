module Server.Core.Domain.Article where

import Data.Bounded (class Ord)
import Data.Date (Date)
import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.UUID (UUID)
import Server.Core.Domain.User (UserId)
import Slug (Slug)

newtype ArticleId = ArticleId UUID

derive instance eqArticleId :: Eq ArticleId
derive instance ordArticleId :: Ord ArticleId

data Article = Article
  { articleId :: ArticleId
  , slug :: Slug
  , title :: String
  , description :: String
  , authorId :: UserId
  , body :: String
  , tagList :: Array String
  , favoritesCount :: Int
  , createdAt :: Date
  , updatedAt :: Maybe Date
  }
