module Conduit.Data.MyUUID
  ( MyUUID(..)
  , parse
  , toString
  , generate
  ) where

import Prelude

import Control.Monad.Except (except, withExceptT)
import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect (Effect)
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype MyUUID = MyUUID UUID

derive instance newtypeMyUUID :: Newtype MyUUID _
derive instance eqMyUUID :: Eq MyUUID
derive instance ordMyUUID :: Ord MyUUID

instance Show MyUUID where
  show (MyUUID uuid) = show uuid

instance WriteForeign MyUUID where
  writeImpl (MyUUID uuid) = writeImpl $ UUID.toString uuid

instance ReadForeign MyUUID where
  readImpl raw = do
    str <- readImpl raw
    withExceptT (\e -> singleton $ ForeignError e) $ except $ parse str

parse :: String -> Either String MyUUID
parse str = case UUID.parseUUID str of
  Nothing -> Left "Invalid UUID"
  Just uuid -> Right $ MyUUID uuid

toString :: MyUUID -> String
toString (MyUUID uuid) = UUID.toString uuid

generate :: Effect MyUUID
generate = MyUUID <$> UUID.genUUID
