module Server.Core.Domain.User
  ( User(..)
  , UserId(..)
  , UserInfo(..)
  , PublicProfile(..)
  , Authentication(..)
  , UserRegistration(..)
  , MetaInfo(..)
  , Username
  , UsernameValidationErrors(..)
  , makeUsername
  ) where

import Prelude

import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.String as String
import Data.UUID (UUID)
import Server.Infra.Data.String (isWordAlphaNum, startsWithLetter)

newtype Username = Username String

derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

newtype UserId = UserId UUID

instance showUsername :: Show Username where
  show (Username username) = "username: " <> username

derive instance eqUserId :: Eq UserId
derive instance ordUserId :: Ord UserId

type UserInfo =
  { username :: Username
  , email :: String
  , bio :: Maybe String
  , image :: Maybe String
  }

type Authentication =
  { token :: String
  }

type UserRegistration =
  { username :: Username
  , email :: String
  , password :: String
  }

type MetaInfo =
  { createdAt :: Date
  , updatedAt :: Maybe Date
  }

type PublicProfile =
  { username :: Username
  , bio :: Maybe String
  , image :: Maybe String
  , following :: Boolean
  }

type User =
  { id :: UserId
  , username :: Username
  , email :: String
  , password :: String
  , bio :: Maybe String
  , image :: Maybe String
  , createdAt :: Date
  , updatedAt :: Maybe Date
  }

data UsernameValidationErrors
  = InvalidCharacters
  | TooShort
  | MustStartWithLetter

makeUsername :: String -> Either UsernameValidationErrors Username
makeUsername username =
  if String.length username < 3 then
    Left TooShort
  else if isWordAlphaNum username then
    Left InvalidCharacters
  else if startsWithLetter username then
    Left MustStartWithLetter
  else
    Right (Username username)
