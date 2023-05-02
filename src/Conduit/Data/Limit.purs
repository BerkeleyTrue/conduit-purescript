module Conduit.Data.Limit
  ( Limit(..)
  , limitToString
  , stringToLimit
  , mkLimit
  ) where

import Prelude

import Data.Either (note, Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Newtype (class Newtype)

newtype Limit = Limit Int

derive instance genericLimit :: Generic Limit _
derive instance newtypeLimit :: Newtype Limit _

instance showLimit :: Show Limit where
  show (Limit x) = show x

limitToString :: Limit -> String
limitToString (Limit x) = show x

stringToLimit :: String -> Either String Limit
stringToLimit val = mkLimit =<< (note "Could not parse" <<< Int.fromString) val

mkLimit :: Int -> Either String Limit
mkLimit = case _ of
  l | l > 0 && l < 1000 -> Right $ Limit l
  _ -> Left "Limit out of bounds"
