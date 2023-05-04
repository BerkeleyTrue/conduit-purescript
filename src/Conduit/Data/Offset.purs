module Conduit.Data.Offset
  ( Offset(..)
  , offsetToString
  , stringToOffset
  , mkOffset
  ) where

import Prelude

import Data.Either (note, Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Newtype (class Newtype)

newtype Offset = Offset Int

derive instance genericLimit :: Generic Offset _
derive instance newtypeLimit :: Newtype Offset _

instance showLimit :: Show Offset where
  show (Offset x) = show x

offsetToString :: Offset -> String
offsetToString (Offset x) = show x

stringToOffset :: String -> Either String Offset
stringToOffset val = mkOffset =<< (note "Could not parse" <<< Int.fromString) val

mkOffset :: Int -> Either String Offset
mkOffset = case _ of
  l | l > 0 && l < 1000 -> Right $ Offset l
  _ -> Left "Offset out of bounds"
