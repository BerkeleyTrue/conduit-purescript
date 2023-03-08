module Server.Core.Ports.Ports where

import Prelude

import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe)
import Server.Core.Domain.Article (Article, ArticleId)
import Server.Core.Domain.Comment (CommentId, Comment)
import Server.Core.Domain.Profile (Profile)
import Server.Core.Domain.User (User, UserId, Username)
import Slug (Slug)

newtype UserRepo m = UserRepo
  { create :: User -> m (Either String User)
  , getById :: UserId -> m (Maybe User)
  , getByUsername :: Username -> m (Maybe User)
  , update :: UserId -> (User -> User) -> m Unit
  }

newtype ProfileRepo m = ProfileRepo
  { get :: UserId -> m (Maybe Profile)
  , update :: UserId -> (Profile -> Profile) -> m Unit
  }

newtype ArticleRepo m = ArticleRepo
  { create :: Article -> m Unit
  , getById :: ArticleId -> m (Maybe Article)
  , getBySlug :: Slug -> m (Maybe Article)
  , list :: m (List Article)
  , update :: ArticleId -> (Article -> Article) -> m Unit
  , delete :: ArticleId -> m Unit
  }

newtype CommentRepo m = CommentRepo
  { create :: Comment -> m Unit
  , getById :: CommentId -> m (Maybe Comment)
  , getByArticle :: ArticleId -> m (Maybe Comment)
  , list :: m (List Comment)
  , update :: CommentId -> (Comment -> Comment) -> m Unit
  , delete :: CommentId -> m Unit
  }
