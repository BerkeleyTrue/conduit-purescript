module Server.Core.Domain.Article where

import Conduit.Data.ArticleId (ArticleId)
import Conduit.Data.MySlug (MySlug)
import Conduit.Data.UserId (AuthorId, UserId)
import Data.JSDate (JSDate)
import Data.Set (Set)
import Data.Maybe (Maybe)

type Tag = String

type FavoritedBy = Set UserId

type Article =
  { articleId :: ArticleId
  , slug :: MySlug
  , title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  , createdAt :: JSDate
  , updatedAt :: Maybe JSDate
  , authorId :: AuthorId
  , favoritedBy :: FavoritedBy
  }
