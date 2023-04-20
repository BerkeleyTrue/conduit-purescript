module Conduit.Data.Int where

import Prelude

import Data.Either (Either, note)
import Data.Int (fromString)

fromString' :: String -> Either String Int
fromString' num = throwIfNotParsed $ fromString num
  where
  throwIfNotParsed = note $ "Expected " <> num <> " to be a number but it was not"
