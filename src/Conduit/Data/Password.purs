module Conduit.Data.Password
  ( Password
  , PasswordValidationErrors
  , mkPassword
  , passwordToString
  , hashPassword
  , comparePasswords
  ) where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Conduit.Data.String (isAnyDigit, isAnyLower, isAnySymbol, isAnyUpper)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.String (split, length)
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (ForeignError(..), fail)
import Node.Buffer (fromString, toString)
import Node.Crypto (randomBytes, timingSafeEqual, scrypt)
import Node.Encoding (Encoding(..))
import Yoga.JSON (class ReadForeign, readImpl)

newtype Password = Password String

derive instance eqPassword :: Eq Password
derive instance ordPassword :: Ord Password

instance ReadForeign Password where
  readImpl json = do
    str <- readImpl json
    let password' = mkPassword str
    case password' of
      Left err -> fail $ ForeignError $ show err
      Right password -> pure password

passwordToString :: Password -> String
passwordToString (Password password) = password

hashPassword :: Password -> Aff String
hashPassword (Password rawPassword) = do
  password <- liftEffect $ fromString rawPassword UTF8

  salt <- liftEffect $ randomBytes 16
  saltString <- liftEffect $ toString Hex salt

  hash <- scrypt password salt 64
  hashString <- liftEffect $ toString Hex hash

  pure $ hashString <> "." <> saltString

comparePasswords :: Password -> Password -> Aff (Either String Boolean)
comparePasswords (Password rawPassword) (Password storedPassword) = runExceptT do
  let (splitten :: List String) = List.fromFoldable $ split (Pattern ".") $ storedPassword

  hashedPassword <- maybeThrow "Expected password to be in format 'hash.salt'" $ List.head splitten

  salt <- maybeThrow "Expected password to be in format 'hash.salt'" $ List.tail splitten >>= List.head
  saltBuf <- liftEffect $ fromString salt Hex

  hashedBuff <- liftEffect $ fromString hashedPassword Hex
  givenHashedBuff <- (liftEffect $ fromString rawPassword UTF8) >>= \given -> liftAff $ scrypt given saltBuf 65

  liftEffect $ timingSafeEqual hashedBuff givenHashedBuff

data PasswordValidationErrors
  = PasswordTooShort
  | PasswordTooLong
  | PasswordMissingLowercase
  | PasswordMissingUppercase
  | PasswordMissingNumber
  | PasswordMissingSpecialChar

instance Show PasswordValidationErrors where
  show PasswordTooShort = "Password must be at least 8 characters long"
  show PasswordTooLong = "Password must be at most 64 characters long"
  show PasswordMissingLowercase = "Password must contain at least one lowercase character"
  show PasswordMissingUppercase = "Password must contain at least one uppercase character"
  show PasswordMissingNumber = "Password must contain at least one number"
  show PasswordMissingSpecialChar = "Password must contain at least one special character"

mkPassword :: String -> Either PasswordValidationErrors Password
mkPassword pass = do
  let passwordLength = length pass
  if passwordLength < 8 then Left PasswordTooShort
  else if passwordLength > 64 then Left PasswordTooLong
  else if (not <<< isAnyLower) pass then Left PasswordMissingLowercase
  else if (not <<< isAnyUpper) pass then Left PasswordMissingUppercase
  else if (not <<< isAnyDigit) pass then Left PasswordMissingNumber
  else if isAnySymbol pass then Left PasswordMissingSpecialChar
  else Right $ Password pass
