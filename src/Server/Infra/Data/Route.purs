module Server.Infra.Data.Route
  ( limitR
  , slugR
  , usernameR
  ) where

import Prelude

import Conduit.Data.Limit (Limit, limitToString, stringToLimit)
import Conduit.Data.Username (Username, mkUsername)
import Data.Bifunctor (lmap)
import Data.Either (note)
import HTTPurple (RouteDuplex', as)
import Slug (Slug)
import Slug as Slug

slugR :: RouteDuplex' String -> RouteDuplex' Slug
slugR = as Slug.toString (Slug.parse >>> note "Invalid slug")

limitR :: RouteDuplex' String -> RouteDuplex' Limit
limitR = as limitToString stringToLimit

usernameR :: RouteDuplex' String -> RouteDuplex' Username
usernameR = as show (lmap show <<< mkUsername)
