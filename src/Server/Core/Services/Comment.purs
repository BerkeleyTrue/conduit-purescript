module Server.Core.Services.Comment
  ( mkCommentService
  , CommentService(..)
  , CommentServiceErrs
  ) where

import Prelude

import Conduit.Data.CommentId (CommentId)
import Conduit.Data.MySlug (MySlug)
import Server.Core.Domain.Comment (Comment)
import Server.Core.Ports.Ports (CommentRepo(..), CommentCreateInput)
import Server.Core.Services.Articles (ArticleService(..))
import Yoga.Om (Om, expandErr)

data Body = String

type CommentServiceErrs r = (articleRepoErr :: String, commentRepoErr :: String | r)

newtype CommentService = CommentService
  { list :: MySlug -> Om {} (CommentServiceErrs ()) (Array Comment)
  , add :: MySlug -> CommentCreateInput -> Om {} (CommentServiceErrs ()) Comment
  , delete :: CommentId -> Om {} (CommentServiceErrs ()) Unit
  }

mkList :: CommentRepo -> ArticleService -> MySlug -> Om {} (CommentServiceErrs ()) (Array Comment)
mkList (CommentRepo { getByArticleId }) (ArticleService { getIdFromSlug }) slug = do
  articleId <- expandErr $ getIdFromSlug slug
  expandErr $ getByArticleId articleId

mkAdd :: CommentRepo -> ArticleService -> MySlug -> CommentCreateInput -> Om {} (CommentServiceErrs ()) Comment
mkAdd (CommentRepo { create }) (ArticleService { getIdFromSlug }) slug input = do
  _ <- expandErr $ getIdFromSlug slug
  expandErr $ create input

mkDelete :: CommentRepo -> CommentId -> Om {} (CommentServiceErrs ()) Unit
mkDelete (CommentRepo { delete }) commentId = do
  expandErr $ delete commentId

mkCommentService :: CommentRepo -> ArticleService -> Om {} () CommentService
mkCommentService commentRepo articleService = do
  pure $ CommentService
    { list: mkList commentRepo articleService
    , add: mkAdd commentRepo articleService
    , delete: mkDelete commentRepo
    }
