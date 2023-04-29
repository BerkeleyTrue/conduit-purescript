module Server.App (route, router) where

import Prelude

import HTTPurple (RouteDuplex', prefix, root, (<+>), type (<+>))
import Server.App.Api (ApiRoute, apiRoute, apiRouter)
import Server.App.Drivers.User (UserRouterExt)
import Server.App.Meta (MetaRoute, metaRoute, metaRouter)
import Server.Infra.HttPurple.Routes (omOrElse)
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.Om (Om, expandCtx)

type AppRoute = ApiRoute <+> MetaRoute

route :: RouteDuplex' AppRoute
route = (root $ prefix "api" apiRoute) <+> metaRoute

router :: forall ext ctx. Om { | ctx } () (OmRouter AppRoute (UserRouterExt ext))
router = expandCtx apiRouter <#> flip omOrElse metaRouter
