module Server.Infra.Data.Route where

import Prelude

import Data.Either (note, Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (maybe)
import HTTPurple (RouteDuplex', as)
import Slug (Slug)
import Slug as Slug

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Invalid slug")

newtype Limit = Limit Int

derive instance genericLimit :: Generic Limit _

limitToString :: Limit -> String
limitToString (Limit x) = show x

stringToLimit :: String -> Either String Limit
stringToLimit val = case (maybe (Left "Could not parse") Right <<< Int.fromString) val of
  Right l | l > 0 && l < 1000 -> Right $ Limit l
  Right _ -> Left "Limit out of bounds"
  Left mess -> Left mess

limit :: RouteDuplex' String -> RouteDuplex' Limit
limit = as limitToString stringToLimit
