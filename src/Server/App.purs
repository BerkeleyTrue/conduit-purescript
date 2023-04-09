module Server.App (route, router) where

import Prelude

import Data.Either (Either)
import Effect.Aff (Aff)
import HTTPurple (Request, ResponseM, RouteDuplex', orElse, prefix, root, (<+>))
import Server.App.Api (ApiRoute, apiRoute, apiRouter)
import Server.App.Meta (MetaRoute, metaRoute, metaRouter)

type Route = Either ApiRoute MetaRoute

route :: RouteDuplex' Route
route = (root $ prefix "api" apiRoute) <+> metaRoute

router :: Aff (Request Route -> ResponseM)
router = do
  apiRouter' <- apiRouter
  pure (apiRouter' `orElse` metaRouter)
