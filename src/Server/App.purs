module Server.App (route, router) where

import Data.Either (Either)
import HTTPurple (ResponseM, RouteDuplex', Request, orElse, (<+>))
import Server.App.Api (ApiRoute, apiRoute, apiRouter)
import Server.App.Meta (MetaRoutes, metaRoute, metaRouter)

type Route = Either ApiRoute MetaRoutes

route :: RouteDuplex' Route
route = apiRoute <+> metaRoute

router :: Request Route -> ResponseM
router = (apiRouter `orElse` metaRouter)
