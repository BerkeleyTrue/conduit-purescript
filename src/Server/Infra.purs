module Server.Infra (omApp) where

import Prelude hiding ((/))

import Data.Maybe (Maybe(..))
import Server.App (route, router)
import Server.Infra.HttPurple (omServer)
import Server.Infra.Yoga.JWT (Secret)
import Yoga.Om (Om)

type AppCtx = { port :: Int, tokenSecret :: Secret }

omApp :: Om AppCtx () Unit
omApp = do
  router' <- router
  omServer route router' Nothing
