module Server.Core.Ports.Ports where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)
import Server.Core.Domain.Article (Article, ArticleId)
import Server.Core.Domain.Comment (CommentId, Comment)
import Server.Core.Domain.Profile (Profile)
import Server.Core.Domain.User (User, UserId, Username)
import Slug (Slug)

class UserRepo m where
  createUser :: User -> m Unit
  getUserById :: UserId -> m (Maybe User)
  getUserByUsername :: Username -> m (Maybe User)
  updateUser :: UserId -> (User -> User) -> m Unit

class ProfileRepo m where
  getProfile :: UserId -> m (Maybe Profile)
  updateProfile :: UserId -> (Profile -> Profile) -> m Unit

class ArticleRepo m where
  createArticle :: Article -> m Unit
  getArticleById :: ArticleId -> m (Maybe Article)
  getArticleBySlug :: Slug -> m (Maybe Article)
  listArticles :: m (List Article)
  updateArticle :: ArticleId -> (Article -> Article) -> m Unit
  deleteArticle :: ArticleId -> m Unit

class CommentRepo m where
  createComment :: Comment -> m Unit
  getCommentById :: CommentId -> m (Maybe Comment)
  getCommentByArticle :: ArticleId -> m (Maybe Comment)
  listComments :: m (List Comment)
  updateComment :: CommentId -> (Comment -> Comment) -> m Unit
  deleteComment :: CommentId -> m Unit
