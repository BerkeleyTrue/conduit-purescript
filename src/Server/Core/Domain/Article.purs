module Server.Core.Domain.Article where

import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.UUID (UUID)
import Server.Core.Domain.User (PublicProfile)
import Slug (Slug)

newtype ArticleId = ArticleId UUID

data Article = Article
  { id :: ArticleId
  , slug :: Slug
  , title :: String
  , description :: String
  , author :: PublicProfile
  , body :: String
  , tagList :: Array String
  , favorited :: Boolean
  , favoritesCount :: Int
  , createdAt :: Date
  , updatedAt :: Maybe Date
  }
