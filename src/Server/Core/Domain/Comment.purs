module Server.Core.Domain.Comment where

import Conduit.Data.ArticleId (ArticleId)
import Conduit.Data.CommentId (CommentId)
import Conduit.Data.UserId (AuthorId)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe)

type Comment =
  { commentId :: CommentId
  , articleId :: ArticleId
  , authorId :: AuthorId
  , body :: String
  , createdAt :: JSDate
  , updatedAt :: Maybe JSDate
  }
