module Conduit.Data.UserId
  ( userIdToString
  , userIdFromString
  , mkUserId
  , UserId
  , AuthorId
  ) where

import Prelude

import Control.Monad.Except (except, withExceptT)
import Data.Bifunctor (rmap)
import Data.Either (Either, note)
import Data.List.NonEmpty (singleton)
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Effect (Effect)
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype UserId = UserId UUID
type AuthorId = UserId

instance ReadForeign UserId where
  readImpl raw = do
    str <- readImpl raw
    withExceptT (\e -> singleton $ ForeignError e) $ except $ userIdFromString str

instance WriteForeign UserId where
  writeImpl (UserId uuid) = writeImpl $ UUID.toString uuid

instance Show UserId where
  show (UserId userId) = show userId

derive instance eqUserId :: Eq UserId
derive instance ordUserId :: Ord UserId

userIdToString :: UserId -> String
userIdToString (UserId uuid) = UUID.toString uuid

userIdFromString :: String -> Either String UserId
userIdFromString = rmap UserId <<< note "Invalid UUId" <<< parseUUID

mkUserId :: Effect UserId
mkUserId = UUID.genUUID <#> UserId
