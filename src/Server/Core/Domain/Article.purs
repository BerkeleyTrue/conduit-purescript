module Server.Core.Domain.Article where

import Conduit.Data.UserId (AuthorId)
import Data.Bounded (class Ord)
import Data.Date (Date)
import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.Show (class Show, show)
import Data.UUID (UUID)
import Slug (Slug)

newtype ArticleId = ArticleId UUID

instance showArticleId :: Show ArticleId where
  show (ArticleId uuid) = show uuid

derive instance eqArticleId :: Eq ArticleId
derive instance ordArticleId :: Ord ArticleId

type Tag = String

type Article =
  { articleId :: ArticleId
  , slug :: Slug
  , title :: String
  , description :: String
  , authorId :: AuthorId
  , body :: String
  , tagList :: Array String
  , favoritesCount :: Int
  , createdAt :: Date
  , updatedAt :: Maybe Date
  }
