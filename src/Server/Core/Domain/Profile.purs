module Server.Core.Domain.Profile (Profile(..)) where

import Data.Bounded (class Ord)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Server.Core.Domain.User (Username)

data Profile = Profile
  { username :: Username
  , bio :: Maybe String
  , image :: Maybe String
  , following :: Boolean
  }

derive instance genericProfile :: Generic Profile _

derive instance eqProfile :: Eq Profile

derive instance ordProfile :: Ord Profile
