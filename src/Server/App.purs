module Server.App (route, router) where

import Prelude

import Data.Either (Either)
import HTTPurple (RouteDuplex', orElse, prefix, root, (<+>))
import Server.App.Api (ApiRoute, apiRoute, apiRouter)
import Server.App.Meta (MetaRoute, metaRoute, metaRouter)
import Server.Infra.HttPurple.Types (Router)
import Yoga.Om (Om, fromAff)

type AppRoute = Either ApiRoute MetaRoute

route :: RouteDuplex' AppRoute
route = (root $ prefix "api" apiRoute) <+> metaRoute

router :: Om {} () (Router AppRoute)
router = fromAff apiRouter <#> flip orElse metaRouter
