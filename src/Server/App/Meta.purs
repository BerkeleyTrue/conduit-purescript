module Server.App.Meta
  ( MetaRoute(..)
  , metaRouter
  , metaRoute
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), RouteDuplex', mkRoute, noArgs, notFound, ok, (/))
import Server.Infra.HttPurple.Types (Router)

data MetaRoute = Ping

derive instance genericMetaRoute :: Generic MetaRoute _

metaRoute :: RouteDuplex' MetaRoute
metaRoute = mkRoute
  { "Ping": "ping" / noArgs
  }

metaRouter :: Router MetaRoute
metaRouter { route: Ping, method: Get } = ok "pong"
metaRouter { route: Ping } = notFound
