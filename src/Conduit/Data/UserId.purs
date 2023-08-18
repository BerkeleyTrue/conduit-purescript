module Conduit.Data.UserId
  ( userIdToString
  , userIdFromString
  , mkUserId
  , UserId(..)
  , AuthorId
  ) where

import Prelude

import Conduit.Data.MyUUID (MyUUID(..))
import Conduit.Data.MyUUID as UUID
import Control.Monad.Except (except, withExceptT)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.List.NonEmpty (singleton)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype UserId = UserId MyUUID
type AuthorId = UserId

derive instance newtypeUserId :: Newtype UserId _

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
userIdFromString = rmap UserId <<< UUID.parse

mkUserId :: Effect UserId
mkUserId = UserId <$> UUID.generate
