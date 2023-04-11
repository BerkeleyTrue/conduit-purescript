module Server.Core.Domain.User
  ( User(..)
  , UserId(..)
  , AuthorId
  , Email
  ) where

import Prelude

import Conduit.Data.Username (Username)
import Data.Date (Date)
import Data.List (List)
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
  , createdAt :: Date
  , updatedAt :: Maybe Date
  }
