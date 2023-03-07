module Server.Infra.Data.String where

import Prelude

import Data.Array (all, head)
import Data.CodePoint.Unicode (isAlpha, isAlphaNum)
import Data.Maybe (maybe)
import Data.String (toCodePointArray)

isWordAlphaNum :: String -> Boolean
isWordAlphaNum = (all isAlphaNum) <<< toCodePointArray

startsWithLetter :: String -> Boolean
startsWithLetter = maybe false isAlpha <<< head <<< toCodePointArray
