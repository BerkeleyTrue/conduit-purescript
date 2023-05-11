module Server.Infra.Data.Route
  ( limitR
  , slugR
  , usernameR
  , offsetR
  , userIdR
  ) where

import Prelude

import Conduit.Data.Limit (Limit, limitToString, stringToLimit)
import Conduit.Data.MySlug (MySlug, parse, toString)
import Conduit.Data.Offset (Offset, offsetToString, stringToOffset)
import Conduit.Data.UserId (UserId, mkUserId)
import Conduit.Data.Username (Username, mkUsername)
import Data.Bifunctor (lmap)
import Data.Either (note)
import HTTPurple (RouteDuplex', as)

slugR :: RouteDuplex' String -> RouteDuplex' MySlug
slugR = as toString (parse >>> note "Invalid slug")

limitR :: RouteDuplex' String -> RouteDuplex' Limit
limitR = as limitToString stringToLimit

offsetR :: RouteDuplex' String -> RouteDuplex' Offset
offsetR = as offsetToString stringToOffset

usernameR :: RouteDuplex' String -> RouteDuplex' Username
usernameR = as show (lmap show <<< mkUsername)

userIdR :: RouteDuplex' String -> RouteDuplex' UserId
userIdR = as show (lmap show <<< mkUserId)
