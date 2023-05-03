module Server.App
  ( route
  , router
  , AppRouterCtx
  , AppRouterExt
  ) where

import Prelude

import HTTPurple (RouteDuplex', prefix, root, (<+>), type (<+>))
import Server.App.Api (ApiRoute, ApiRouterExt, ApiRouterCtx, apiRoute, apiRouter)
import Server.App.Meta (MetaRoute, metaRoute, metaRouter)
import Server.Infra.HttPurple.Routes (omOrElse)
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.Om (Om)

type AppRoute = ApiRoute <+> MetaRoute

route :: RouteDuplex' AppRoute
route = (root $ prefix "api" apiRoute) <+> metaRoute

type AppRouterExt ext = ApiRouterExt ext
type AppRouterCtx ctx = ApiRouterCtx ctx

router :: forall ext ctx. Om (AppRouterCtx ctx) () (OmRouter AppRoute (AppRouterExt ext))
router = apiRouter <#> flip omOrElse metaRouter
