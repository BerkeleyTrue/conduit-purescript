module Server.Infra.Node.Process where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Either (Either, note)
import Effect (Effect)
import Node.Process (lookupEnv)

lookupEnv' :: String -> Effect (Either String String)
lookupEnv' env = throwIfNotFound <$> lookupEnv env
  where
  throwIfNotFound = note $ "Expected to find " <> env <> " in the environment but found nothing."

lookupEnv_ :: String -> ExceptT String Effect String
lookupEnv_ = ExceptT <<< lookupEnv'
