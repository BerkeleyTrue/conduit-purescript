module Conduit.Data.Int where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Either (Either, note)
import Data.Int (fromString)

fromString' :: String -> Either String Int
fromString' num = throwIfNotParsed $ fromString num
  where
  throwIfNotParsed = note $ "Expected " <> num <> " to be a number but it was not"

fromString_ :: forall m. Monad m => String -> ExceptT String m Int
fromString_ = ExceptT <<< pure <<< fromString'
