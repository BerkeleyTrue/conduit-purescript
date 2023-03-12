module Server.Core.Domain.Comment where

import Data.Date (Date)
import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.Ord (class Ord)
import Data.Show (class Show, show)
import Data.UUID (UUID)
import Server.Core.Domain.Article (ArticleId)
import Server.Core.Domain.User (UserId)

newtype CommentId = CommentId UUID

instance showCommentId :: Show CommentId where
  show (CommentId uuid) = show uuid

derive instance eqCommentId :: Eq CommentId
derive instance ordCommentId :: Ord CommentId

data Comment = Comment
  { commentId :: CommentId
  , articleId :: ArticleId
  , authorId :: UserId
  , body :: String
  , createdAt :: Date
  , updatedAt :: Maybe Date
  }
