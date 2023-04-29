module Server.Infra.HttPurple.Server
  ( ExceptionHandler
  , omEnhanceRouter
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect.Console (error)
import Effect.Class (liftEffect)
import Effect.Exception (Error, message)
import HTTPurple (ResponseM, internalServerError)
import Server.Infra.HttPurple.Types (OmRouter, ExtRouter)
import Yoga.Om (runOm)

type ExceptionHandler = Error -> ResponseM

defaultOnError :: ExceptionHandler
defaultOnError = \err -> do
  liftEffect $ error $ message err
  internalServerError "Internal server error."

-- | Convert a Router that response in Om to a Router that response in Aff
omEnhanceRouter :: forall route ext. (Maybe ExceptionHandler) -> (OmRouter route ext) -> (ExtRouter route ext)
omEnhanceRouter handler router = runOm {} { exception: fromMaybe defaultOnError  handler } <<< router
