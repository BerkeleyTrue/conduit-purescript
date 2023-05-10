module Conduit.Data.UserId where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Control.Monad.Except (except, runExcept, withExceptT)
import Data.Either (Either)
import Data.List.NonEmpty (singleton)
import Data.UUID (UUID, parseUUID)
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, readImpl)

newtype UserId = UserId UUID
type AuthorId = UserId

mkUserId :: String -> Either String UserId
mkUserId str = runExcept do
  uuid <- maybeThrow "Invalid UUId" $ parseUUID str
  pure $ UserId uuid

instance ReadForeign UserId where
  readImpl raw = do
    str <- readImpl raw
    withExceptT (\e -> singleton $ ForeignError e) $ except $ mkUserId str

instance showUserId :: Show UserId where
  show (UserId userId) = show userId

derive instance eqUserId :: Eq UserId
derive instance ordUserId :: Ord UserId
