module Server.Core.Domain.User
  ( User(..)
  , UserId(..)
  , AuthorId
  , Email
  , Username
  , UsernameValidationErrors(..)
  , makeUsername
  ) where

import Prelude

import Conduit.Data.String (isWordAlphaNum, startsWithLetter)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe)
import Data.String as String
import Data.UUID (UUID)
import Foreign (ForeignError(..), fail)
import Yoga.JSON (class ReadForeign, readImpl)

newtype Username = Username String

derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

instance showUsername :: Show Username where
  show (Username username) = "username: " <> username

instance ReadForeign Username where
  readImpl json = do
    (username :: String) <- readImpl json
    let username' = makeUsername username
    case username' of
      Left err -> fail $ ForeignError $ show err
      Right username'' -> pure username''

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

data UsernameValidationErrors
  = InvalidCharacters
  | TooShort
  | MustStartWithLetter

instance showUsernameValidationErrors :: Show UsernameValidationErrors where
  show InvalidCharacters = "Username must only contain letters and numbers"
  show TooShort = "Username must be at least 3 characters long"
  show MustStartWithLetter = "Username must start with a letter"

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
