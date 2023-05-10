module Server.Core.Domain.Article where

import Conduit.Data.MySlug (MySlug)
import Conduit.Data.UserId (AuthorId)
import Data.Bounded (class Ord)
import Data.Eq (class Eq)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe)
import Data.Show (class Show, show)
import Data.UUID (UUID)

newtype ArticleId = ArticleId UUID

instance showArticleId :: Show ArticleId where
  show (ArticleId uuid) = show uuid

derive instance eqArticleId :: Eq ArticleId
derive instance ordArticleId :: Ord ArticleId

type Tag = String

type Article =
  { articleId :: ArticleId
  , slug :: MySlug
  , title :: String
  , description :: String
  , authorId :: AuthorId
  , body :: String
  , tagList :: Array String
  , favoritesCount :: Int
  , createdAt :: JSDate
  , updatedAt :: Maybe JSDate
  }
