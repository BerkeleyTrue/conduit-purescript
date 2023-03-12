module Server.Core.Ports.Ports where

import Prelude

import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe)
import Server.Core.Domain.Article (Article, ArticleId)
import Server.Core.Domain.Comment (CommentId, Comment)
import Server.Core.Domain.User (User, UserId, Username)
import Slug (Slug)

type UserCreateInput =
  { username :: Username
  , email :: String
  , password :: String
  }

newtype UserRepo m = UserRepo
  { create :: UserCreateInput -> m (Either String User)
  , getById :: UserId -> m (Maybe User)
  , getByUsername :: Username -> m (Maybe User)
  , update :: UserId -> (User -> User) -> m (Either String User)
  }

type ArticleCreateInput =
  { title :: String
  , description :: String
  , body :: String
  , tagList :: Maybe (List String)
  , authorId :: UserId
  }

newtype ArticleRepo m = ArticleRepo
  { create :: ArticleCreateInput -> m (Either String Article)
  , getById :: ArticleId -> m (Either String Article)
  , getBySlug :: Slug -> m (Either String Article)
  , list :: m (List Article)
  , update :: ArticleId -> (Article -> Article) -> m (Either String Article)
  , delete :: ArticleId -> m (Either String Unit)
  }

newtype CommentRepo m = CommentRepo
  { create :: Comment -> m (Either String Unit)
  , getById :: CommentId -> m (Maybe Comment)
  , getByArticle :: ArticleId -> m (Maybe Comment)
  , list :: m (List Comment)
  , update :: CommentId -> (Comment -> Comment) -> m Unit
  , delete :: CommentId -> m Unit
  }
