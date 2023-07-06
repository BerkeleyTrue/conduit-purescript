module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Test.Conduit.Data.Password (passwordSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "main" do
     passwordSpec
