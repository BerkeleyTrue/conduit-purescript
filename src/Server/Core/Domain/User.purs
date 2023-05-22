module Server.Core.Domain.User
  ( User(..)
  , Author
  , Email
  ) where

import Conduit.Data.Password (HashedPassword)
import Conduit.Data.UserId (UserId, AuthorId)
import Conduit.Data.Username (Username)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe)

type Email = String

type User =
  { userId :: UserId
  , username :: Username
  , email :: Email
  , password :: HashedPassword
  , following :: Array AuthorId
  , bio :: Maybe String
  , image :: Maybe String
  , createdAt :: JSDate
  , updatedAt :: Maybe JSDate
  }

type Author = User
