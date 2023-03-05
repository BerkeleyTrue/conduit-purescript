module Server.App.Api.User
  ( UserRoute(..)
  , userRoute
  , userRouter
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', noArgs, notFound, ok, prefix, sum, (/))

data UserRoute = Authen | User

derive instance genericUserRoute :: Generic UserRoute _

userRoute :: RouteDuplex' UserRoute
userRoute = prefix "user" $ sum
  { "Authen": "login" / noArgs
  , "User": noArgs
  }

userRouter :: Request UserRoute -> ResponseM
userRouter { route: Authen, method: Post } = ok "authenticate"
userRouter { route: Authen } = notFound

userRouter { route: User, method: Post } = ok "Create user" -- no auth
userRouter { route: User, method: Put } = ok "Update user"
userRouter { route: User, method: Get } = ok "Find user"
userRouter { route: User } = notFound
