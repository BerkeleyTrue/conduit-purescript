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

newtype UserInfo = UserInfo
  { username :: Username
  , email :: String
  , bio :: Maybe String
  , image :: Maybe String
  }

newtype Authentication = Authentication
  { token :: String
  }

newtype UserRegistration = UserRegistration
  { username :: Username
  , email :: String
  , password :: String
  }

newtype MetaInfo = MetaInfo
  { createdAt :: Date
  , updatedAt :: Maybe Date
  }

newtype PublicProfile = Profile
  { username :: Username
  , bio :: Maybe String
  , image :: Maybe String
  }

data User =
  User
    UserId
    UserInfo
    (Maybe Authentication)
    (Maybe UserRegistration)
    (Maybe MetaInfo)

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
