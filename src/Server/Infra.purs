module Server.Infra (createApp) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', catchAll, mkRoute, noArgs, notFound, ok, response, (/))
import HTTPurple.Status as Status
import Server.Infra.HttPurple (createServer)

data Route
  = Home
  | Ping
  | CatchAll (Array String)

derive instance genericRoute :: Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Ping": "ping" / noArgs
  , "CatchAll": catchAll
  }

router :: Request Route -> ResponseM
router { route: Home, method: Get } = ok "hello world!"
router { route: Ping, method: Get } = ok "pong"
router { route: CatchAll _ } = response Status.notFound "Opps, something went wrong!"
router _ = notFound

createApp :: Int -> Effect Unit
createApp port = createServer route router port
