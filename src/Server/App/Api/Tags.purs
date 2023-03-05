module Server.App.Api.Tags (TagsRoute(..), tagsRoute, tagsRouter) where

import Prelude

import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), RouteDuplex', noArgs, notFound, ok, prefix, sum)
import Server.Infra.HttPurple.Types (Router)

data TagsRoute = List

derive instance genericTagRoute :: Generic TagsRoute _

tagsRoute :: RouteDuplex' TagsRoute
tagsRoute = prefix "tags" $ sum { "List": noArgs }


tagsRouter :: Router TagsRoute
tagsRouter { route: List, method: Get } = ok "Get tags"
tagsRouter { route: List } = notFound
