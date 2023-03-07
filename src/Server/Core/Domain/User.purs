module Server.Core.Domain.User
  ( User(..)
  , Username
  , UsernameValidationErrors(..)
  , makeUsername
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.String as String
import Server.Infra.Data.String (isWordAlphaNum, startsWithLetter)

newtype Username = Username String

derive instance eqUsername :: Eq Username

instance showUsername :: Show Username where
  show (Username username) = "username: " <> username

data UsernameValidationErrors
  = InvalidCharacters
  | TooShort
  | MustStartWithLetter

data User = User
  { email :: String
  , username :: Username
  , bio :: String
  , image :: Maybe String
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
