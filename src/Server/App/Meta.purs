module Server.App.Meta (MetaRoute(..), metaRouter, metaRoute) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', mkRoute, noArgs, notFound, ok, (/))

data MetaRoute = Ping

derive instance genericMetaRoute :: Generic MetaRoute _

metaRoute :: RouteDuplex' MetaRoute
metaRoute = mkRoute
  { "Ping": "ping" / noArgs
  }

metaRouter :: Request MetaRoute -> ResponseM
metaRouter { route: Ping, method: Get } = ok "pong"
metaRouter { route: Ping } = notFound
