module Server.Infra.HttPurple.Server
  ( ExceptionHandler
  , omEnhanceRouter
  ) where

import Prelude

import Effect.Exception (Error)
import HTTPurple (ResponseM)
import Server.Infra.HttPurple.Types (OmRouter, ExtRouter)
import Yoga.Om (runOm)

type ExceptionHandler = Error -> ResponseM

-- | Convert a Router that response in Om to a Router that response in Aff
omEnhanceRouter :: forall route ext. ExceptionHandler -> (OmRouter route ext) -> (ExtRouter route ext)
omEnhanceRouter handler router = runOm {} { exception: handler } <<< router
