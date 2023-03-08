module Server.Core.Domain.User
  ( User(..)
  , Username
  , UserId
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

data UsernameValidationErrors
  = InvalidCharacters
  | TooShort
  | MustStartWithLetter

data User = User
  { id :: UserId
  , username :: Username
  , email :: String
  , bio :: String
  , image :: Maybe String
  , createdAt :: Date
  , updatedAt :: Date
  }

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
