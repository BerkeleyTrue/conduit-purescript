module Conduit.Data.ArticleId where

import Prelude

import Data.UUID (UUID, toString)
import Yoga.JSON (class WriteForeign, writeImpl)

newtype ArticleId = ArticleId UUID

instance showArticleId :: Show ArticleId where
  show (ArticleId uuid) = show uuid

derive instance eqArticleId :: Eq ArticleId
derive instance ordArticleId :: Ord ArticleId

instance WriteForeign ArticleId where
  writeImpl (ArticleId uuid) = writeImpl (toString uuid)
