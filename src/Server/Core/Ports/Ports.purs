module Server.Core.Ports.Ports where

import Prelude

import Conduit.Data.Limit (Limit)
import Conduit.Data.Offset (Offset)
import Conduit.Data.UserId (UserId, AuthorId)
import Conduit.Data.Username (Username)
import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Server.Core.Domain.Article (Article, ArticleId, Tag)
import Server.Core.Domain.Comment (CommentId, Comment)
import Server.Core.Domain.User (Email, User)
import Slug (Slug)
import Yoga.Om (Om)

type UserCreateInput =
  { username :: Username
  , email :: String
  , password :: String
  }

newtype UserRepo = UserRepo
  { create :: UserCreateInput -> Om {} (userRepoErr :: String) User
  , getById :: UserId -> Om {} (userRepoErr :: String) User
  , getByUsername :: Username -> Om {} (userRepoErr :: String) User
  , getByEmail :: Email -> Om {} (userRepoErr :: String) User
  , update :: UserId -> (User -> User) -> Om {} (userRepoErr :: String) User
  , follow :: UserId -> AuthorId -> Om {} (userRepoErr :: String) User
  , unfollow :: UserId -> AuthorId -> Om {} (userRepoErr :: String) User
  }

derive instance newtypeUserRepo :: Newtype UserRepo _

type ArticleCreateInput =
  { title :: String
  , description :: String
  , body :: String
  , tagList :: Maybe (List String)
  , authorId :: UserId
  }

type ArticleListInput =
  { tag :: Maybe Tag
  , author :: Maybe AuthorId
  , favorited :: Maybe AuthorId
  , limit :: Maybe Limit
  , offset :: Maybe Offset
  }

newtype ArticleRepo = ArticleRepo
  { create :: ArticleCreateInput -> Om {} (articleRepoErr :: String) Article
  , getById :: ArticleId -> Om {} (articleRepoErr :: String) Article
  , getBySlug :: Slug -> Om {} (articleRepoErr :: String) Article
  , list :: ArticleListInput -> Om {} (articleRepoErr :: String) (List Article)
  , update :: ArticleId -> (Article -> Article) -> Om {} (articleRepoErr :: String) Article
  , delete :: ArticleId -> Om {} (articleRepoErr :: String) Unit
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
