module Conduit.Data.MySlug
  ( MySlug
  , generate
  , parse
  , toString
  ) where

import Prelude

import Data.Maybe (Maybe)
import Slug (Slug)
import Slug as Slug
import Yoga.JSON (class WriteForeign, writeImpl)

newtype MySlug = MySlug Slug

instance WriteForeign MySlug where
  writeImpl (MySlug slug) = writeImpl $ Slug.toString slug

instance Show MySlug where
  show (MySlug slug) = show slug

derive newtype instance Eq MySlug
derive newtype instance Ord MySlug
derive newtype instance Semigroup MySlug

generate :: String -> Maybe MySlug
generate = (MySlug <$> _) <<< Slug.generate

parse :: String -> Maybe MySlug
parse = (MySlug <$> _) <<< Slug.parse

toString :: MySlug -> String
toString (MySlug slug) = Slug.toString slug
