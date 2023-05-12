module Conduit.Data.CommentId
  ( CommentId
  , mkCommentId
  , commentIdToString
  , commentIdFromString
  ) where

import Prelude

import Data.Bifunctor (rmap)
import Data.Either (Either, note)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect (Effect)
import Yoga.JSON (class WriteForeign, writeImpl)

newtype CommentId = CommentId UUID

instance Show CommentId where
  show (CommentId uuid) = show uuid

derive instance eqArticleId :: Eq CommentId
derive instance ordArticleId :: Ord CommentId

instance WriteForeign CommentId where
  writeImpl (CommentId uuid) = writeImpl (UUID.toString uuid)

commentIdToString :: CommentId -> String
commentIdToString (CommentId uuid) = UUID.toString uuid

commentIdFromString :: String -> Either String CommentId
commentIdFromString = rmap CommentId <<< note "Could not parse Comment Id" <<< UUID.parseUUID

mkCommentId :: Effect CommentId
mkCommentId = UUID.genUUID <#> CommentId
