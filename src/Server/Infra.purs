module Server.Infra (omApp) where

import Prelude hiding ((/))

import Server.App (route, router)
import Server.Infra.HttPurple (omServer)
import Yoga.Om (Om, widenCtx)

type AppCtx = { port :: Int }

omApp :: Om AppCtx () Unit
omApp = do
  router' <- router
  widenCtx { router: router', route } omServer
