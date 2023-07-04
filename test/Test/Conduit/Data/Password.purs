module Test.Test.Conduit.Data.Password where

import Prelude

import Conduit.Data.Password (comparePasswords, hashPassword, mkPassword)
import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

passwordSpec :: Spec Unit
passwordSpec = do
  describe "Conduit.Data.Password" do
    it "should match password" do
      case mkPassword "Password1423" of
        Left err -> fail $ (show err)
        Right password -> do
          hashed <- hashPassword password
          eith <- comparePasswords password hashed
          eith `shouldEqual` Right true
