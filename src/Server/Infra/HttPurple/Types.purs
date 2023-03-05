module Server.Infra.HttPurple.Types
  (Router(..)) where

import HTTPurple (Request, ResponseM)

type Router a = Request a -> ResponseM
