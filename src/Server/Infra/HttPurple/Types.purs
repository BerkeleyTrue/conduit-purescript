module Server.Infra.HttPurple.Types
  ( Router(..)
  , Middleware'
  ) where

import HTTPurple (Request, ResponseM)

type Router a = Request a -> ResponseM

type Middleware' route = Router route -> Router route
