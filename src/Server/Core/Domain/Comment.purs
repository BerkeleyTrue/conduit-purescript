module Server.Core.Domain.Comment where

import Conduit.Data.ArticleId (ArticleId)
import Conduit.Data.UserId (AuthorId)
import Data.Eq (class Eq)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe)
import Data.Ord (class Ord)
import Data.Show (class Show, show)
import Data.UUID (UUID)

newtype CommentId = CommentId UUID

instance showCommentId :: Show CommentId where
  show (CommentId uuid) = show uuid

derive instance eqCommentId :: Eq CommentId
derive instance ordCommentId :: Ord CommentId

data Comment = Comment
  { commentId :: CommentId
  , articleId :: ArticleId
  , authorId :: AuthorId
  , body :: String
  , createdAt :: JSDate
  , updatedAt :: Maybe JSDate
  }
