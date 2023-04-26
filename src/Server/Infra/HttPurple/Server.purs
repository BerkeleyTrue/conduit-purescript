module Server.Infra.HttPurple.Server
  ( ExceptionHandler
  , omToRouter
  ) where

import Prelude

import Effect.Exception (Error)
import HTTPurple (ResponseM)
import Server.Infra.HttPurple.Types (OmRouter, Router)
import Yoga.Om (runOm)

type ExceptionHandler = Error -> ResponseM

-- | Convert a Router that response in Om to a Router that response in Aff
omToRouter :: forall route. ExceptionHandler -> (OmRouter route) -> (Router route)
omToRouter handler router = runOm {} { exception: handler } <<< router
