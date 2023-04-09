module Server.Infra (createApp) where

import Prelude hiding ((/))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Server.App (route, router)
import Server.Infra.HttPurple (createServer)

createApp :: Int -> Aff Unit
createApp port = do
  router' <- router
  liftEffect $ createServer route router' port
