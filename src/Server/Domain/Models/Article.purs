module Server.Domain.Models.Article where

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
