module Server.Infra (createApp) where

import Prelude hiding ((/))

import Effect (Effect)
import Server.App (route, router)
import Server.Infra.HttPurple (createServer)

createApp :: Int -> Effect Unit
createApp port = createServer route router port
