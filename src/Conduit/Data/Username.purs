module Conduit.Data.Username
  ( Username(..)
  , Authorname
  , UsernameValidationErrors(..)
  , mkUsername
  ) where

import Prelude

import Conduit.Data.String (isWordAlphaNum, startsWithLetter)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.String as String
import Foreign (ForeignError(..), fail)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype Username = Username String
type Authorname = Username

derive instance genericUsername :: Generic Username _
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username
instance writeForiegnUsername :: WriteForeign Username
  where
    writeImpl (Username username) = writeImpl username

instance showUsername :: Show Username where
  show (Username username) = username

instance ReadForeign Username where
  readImpl json = do
    (username :: String) <- readImpl json
    let username' = mkUsername username
    case username' of
      Left err -> fail $ ForeignError $ show err
      Right username'' -> pure username''

data UsernameValidationErrors
  = InvalidCharacters
  | TooShort
  | MustStartWithLetter

instance showUsernameValidationErrors :: Show UsernameValidationErrors where
  show InvalidCharacters = "Username must only contain letters and numbers"
  show TooShort = "Username must be at least 3 characters long"
  show MustStartWithLetter = "Username must start with a letter"

mkUsername :: String -> Either UsernameValidationErrors Username
mkUsername username =
  if String.length username < 3 then
    Left TooShort
  else if not (isWordAlphaNum username) then
    Left InvalidCharacters
  else if not (startsWithLetter username) then
    Left MustStartWithLetter
  else
    Right (Username username)
