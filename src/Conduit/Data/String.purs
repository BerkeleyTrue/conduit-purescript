module Conduit.Data.String where

import Prelude

import Data.Array (all, head, any)
import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isDecDigit, isLower, isSymbol)
import Data.Maybe (maybe)
import Data.String (toCodePointArray)

isWordAlphaNum :: String -> Boolean
isWordAlphaNum = (all isAlphaNum) <<< toCodePointArray

startsWithLetter :: String -> Boolean
startsWithLetter = maybe false isAlpha <<< head <<< toCodePointArray

isAnyLower :: String -> Boolean
isAnyLower = any isLower <<< toCodePointArray

isAnyUpper :: String -> Boolean
isAnyUpper = any (not <<< isLower) <<< toCodePointArray

isAnyDigit :: String -> Boolean
isAnyDigit = any isDecDigit <<< toCodePointArray

isAnySymbol :: String -> Boolean
isAnySymbol = any isSymbol <<< toCodePointArray
