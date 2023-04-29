module Server.Core.Domain.User
  ( User(..)
  , Author
  , UserId(..)
  , AuthorId
  , Email
  ) where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Conduit.Data.Username (Username)
import Data.JSDate (JSDate)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe)
import Data.UUID (UUID, parseUUID)
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, readImpl)

newtype UserId = UserId UUID

instance ReadForeign UserId where
  readImpl raw = do
    str <- readImpl raw
    uuid <- maybeThrow (singleton $ ForeignError "Invalid UUId") $ parseUUID str
    pure $ UserId uuid

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
  , following :: Array AuthorId
  , bio :: Maybe String
  , image :: Maybe String
  , createdAt :: JSDate
  , updatedAt :: Maybe JSDate
  }

type Author = User
