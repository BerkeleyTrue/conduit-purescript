module Conduit.App.Winston
  ( Level(..)
  , Logger(..)
  , Namespace
  , mkLogger
  ) where

import Prelude

import Effect (Effect)

data Level
  = Error
  | Warning
  | Info
  | Debug

type Namespace = String

type Logger =
  { error :: String -> Effect Unit
  , warn :: String -> Effect Unit
  , info :: String -> Effect Unit
  , debug :: String -> Effect Unit
  }

foreign import mkLogger :: Level -> Namespace -> Effect Logger
