module Server.Core.Domain.User
  ( User(..)
  , Author
  , UserId(..)
  , AuthorId
  , Email
  ) where

import Prelude

import Conduit.Data.Username (Username)
import Data.List (List)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe)
import Data.UUID (UUID)


newtype UserId = UserId UUID

type AuthorId = UserId

instance showUserId :: Show UserId where
  show (UserId userId) = "userId: " <> show userId

derive instance eqUserId :: Eq UserId
derive instance ordUserId :: Ord UserId

type Email = String

type User =
  { userId :: UserId
  , username :: Username
  , email :: Email
  , password :: String
  , following :: List AuthorId
  , bio :: Maybe String
  , image :: Maybe String
  , createdAt :: JSDate
  , updatedAt :: Maybe JSDate
  }

type Author = User
