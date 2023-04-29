module Server.Infra.HttPurple.Types
  ( ExtRouter
  , Router
  , OmResponse
  , OmRouter
  ) where

import HTTPurple (Response, ResponseM, ExtRequest)
import Yoga.Om (Om)

type Router route = ExtRequest route () -> ResponseM
type ExtRouter route ext = ExtRequest route ext -> ResponseM

type OmResponse = Om {} () Response

type OmRouter route ext = ExtRequest route ext -> OmResponse
