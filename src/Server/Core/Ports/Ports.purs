module Server.Core.Ports.Ports where

import Prelude

import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe)
import Server.Core.Domain.Article (Article, ArticleId)
import Server.Core.Domain.Comment (CommentId, Comment)
import Server.Core.Domain.User (Email, User, UserId, Username, AuthorId)
import Slug (Slug)

type UserCreateInput =
  { username :: Username
  , email :: String
  , password :: String
  }

newtype UserRepo m = UserRepo
  { create :: UserCreateInput -> m (Either String User)
  , getById :: UserId -> m (Either String User)
  , getByUsername :: Username -> m (Either String User)
  , getByEmail :: Email -> m (Either String User)
  , update :: UserId -> (User -> User) -> m (Either String User)
  , follow :: UserId -> AuthorId -> m (Either String User)
  , unfollow :: UserId -> AuthorId -> m (Either String User)
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

type CommentCreateInput =
  { body :: String
  , authorId :: UserId
  , articleId :: ArticleId
  }

newtype CommentRepo m = CommentRepo
  { create :: CommentCreateInput -> m (Either String Comment)
  , getById :: CommentId -> m (Either String Comment)
  , getByArticleId :: ArticleId -> m (Either String (List Comment))
  , list :: m (List Comment)
  , update :: CommentId -> (Comment -> Comment) -> m (Either String Comment)
  , delete :: CommentId -> m (Either String Unit)
  }
