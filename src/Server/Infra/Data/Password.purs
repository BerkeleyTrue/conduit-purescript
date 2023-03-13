module Server.Infra.Data.Password where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Control.Monad.Except (runExceptT)
import Data.Either (Either)
import Data.List (List)
import Data.List as List
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.Buffer (fromString, toString)
import Node.Crypto (randomBytes, timingSafeEqual, scrypt)
import Node.Encoding (Encoding(..))

hashPassword :: String -> Aff String
hashPassword rawPassword = do
  password <- liftEffect $ fromString rawPassword UTF8

  salt <- liftEffect $ randomBytes 16
  saltString <- liftEffect $ toString Hex salt

  hash <- scrypt password salt 64
  hashString <- liftEffect $ toString Hex hash

  pure $ hashString <> "." <> saltString

comparePasswords :: String -> String -> Aff (Either String Boolean)
comparePasswords rawPassword storedPassword = runExceptT do
  let (splitten :: List String) = List.fromFoldable $ split (Pattern ".") storedPassword

  hashedPassword <- maybeThrow "Expected password to be in format 'hash.salt'" $ List.head splitten

  salt <- maybeThrow "Expected password to be in format 'hash.salt'" $ List.tail splitten >>= List.head
  saltBuf <- liftEffect $ fromString salt Hex

  hashedBuff <- liftEffect $ fromString hashedPassword Hex
  givenHashedBuff <- (liftEffect $ fromString rawPassword UTF8) >>= \given -> liftAff $ scrypt given saltBuf 65

  liftEffect $ timingSafeEqual hashedBuff givenHashedBuff
