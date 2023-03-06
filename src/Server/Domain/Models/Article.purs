module Server.Domain.Models.Article where

import Data.Bounded (class Ord)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (compare)
import Server.Domain.Models.Profile (Profile)

data Article = Article
  { slug :: String
  , title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  , createdAt :: String
  , updatedAt :: String
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: Profile
  }

derive instance genericArticle :: Generic Article _

derive instance eqArticle :: Eq Article

instance ordArticle :: Ord Article where
  compare (Article { slug: s1 }) (Article { slug: s2 }) = compare s1 s2
