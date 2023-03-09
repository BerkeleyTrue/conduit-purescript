module Server.Core.Domain.Comment where

import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.UUID (UUID)
import Server.Core.Domain.Article (ArticleId)
import Server.Core.Domain.User (PublicProfile)

newtype CommentId = CommentId UUID

data Comment = Comment
  { commentId :: CommentId
  , articleId :: ArticleId
  , body :: String
  , author :: PublicProfile
  , createAt :: Date
  , updateAt :: Maybe Date
  }
