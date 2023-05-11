module Server.Core.Domain.Article where

import Conduit.Data.ArticleId (ArticleId)
import Conduit.Data.MySlug (MySlug)
import Conduit.Data.UserId (AuthorId)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe)

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
