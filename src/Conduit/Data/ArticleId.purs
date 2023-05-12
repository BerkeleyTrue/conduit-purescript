module Conduit.Data.ArticleId
  ( ArticleId
  , articleIdToString
  , articleIdFromString
  , mkArticleId
  ) where

import Prelude

import Data.Bifunctor (rmap)
import Data.Either (Either, note)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect (Effect)
import Yoga.JSON (class WriteForeign, writeImpl)

newtype ArticleId = ArticleId UUID

instance Show ArticleId where
  show (ArticleId uuid) = show uuid

derive instance eqArticleId :: Eq ArticleId
derive instance ordArticleId :: Ord ArticleId

instance WriteForeign ArticleId where
  writeImpl (ArticleId uuid) = writeImpl (UUID.toString uuid)

articleIdToString :: ArticleId -> String
articleIdToString (ArticleId uuid) = UUID.toString uuid

articleIdFromString :: String -> Either String ArticleId
articleIdFromString = rmap ArticleId <<< note "Could not parse ArticleId" <<< UUID.parseUUID

mkArticleId :: Effect ArticleId
mkArticleId = UUID.genUUID <#> ArticleId
