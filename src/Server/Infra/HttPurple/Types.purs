module Server.Infra.HttPurple.Types
  ( Router
  , Middleware'
  , OmResponse
  , OmRouter
  ) where

import HTTPurple (Request, Response, ResponseM)
import Yoga.Om (Om)

type Router a = Request a -> ResponseM

type Middleware' route = Router route -> Router route

type OmResponse = Om {} () Response

type OmRouter a = Request a -> OmResponse
