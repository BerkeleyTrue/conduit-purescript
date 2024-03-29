module Server.Core.Ports.Ports where

import Prelude

import Conduit.Data.ArticleId (ArticleId)
import Conduit.Data.CommentId (CommentId)
import Conduit.Data.Limit (Limit)
import Conduit.Data.MySlug (MySlug)
import Conduit.Data.Offset (Offset)
import Conduit.Data.Password (HashedPassword)
import Conduit.Data.UserId (UserId, AuthorId)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Server.Core.Domain.Article (Article, Tag)
import Server.Core.Domain.Comment (Comment)
import Server.Core.Domain.User (Email, User)
import Yoga.Om (Om)

type UserCreateInput r =
  { username :: Username
  , email :: String
  | r
  }

newtype UserRepo = UserRepo
  { create :: UserCreateInput (password :: HashedPassword) -> Om {} (userRepoErr :: String) User
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
  , tagList :: Maybe (Array String)
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
  , getBySlug :: MySlug -> Om {} (articleRepoErr :: String) Article
  , list :: ArticleListInput -> Om {} (articleRepoErr :: String) (Array Article)
  , update :: MySlug -> (Article -> Article) -> Om {} (articleRepoErr :: String) Article
  , delete :: MySlug -> Om {} (articleRepoErr :: String) Unit
  }

type CommentCreateInput =
  { body :: String
  , authorId :: UserId
  , articleId :: ArticleId
  }

newtype CommentRepo = CommentRepo
  { create :: CommentCreateInput -> Om {} (commentRepoErr :: String) Comment
  , getById :: CommentId -> Om {} (commentRepoErr :: String) Comment
  , getByArticleId :: ArticleId -> Om {} (commentRepoErr :: String) (Array Comment)
  , update :: CommentId -> (Comment -> Comment) -> Om {} (commentRepoErr :: String) Comment
  , delete :: CommentId -> Om {} (commentRepoErr :: String) Unit
  }
