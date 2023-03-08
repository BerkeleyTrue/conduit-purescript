module Server.Core.Domain.Article where

import Data.Bounded (class Ord)
import Data.Date (Date)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Ord (compare)
import Data.UUID (UUID)
import Server.Core.Domain.Profile (Profile)
import Slug (Slug)

newtype ArticleId = ArticleId UUID

derive instance eqArticleId :: Eq ArticleId

data Article = Article
  { id :: ArticleId
  , slug :: Slug
  , title :: String
  , description :: String
  , author :: Profile
  , body :: String
  , tagList :: Array String
  , favorited :: Boolean
  , favoritesCount :: Int
  , createdAt :: Date
  , updatedAt :: Maybe Date
  }

derive instance genericArticle :: Generic Article _

derive instance eqArticle :: Eq Article

instance ordArticle :: Ord Article where
  compare (Article { slug: s1 }) (Article { slug: s2 }) = compare s1 s2
