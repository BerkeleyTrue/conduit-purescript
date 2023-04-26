module Conduit.Yoga.Om (fromAffThrowLeftAsOm) where

import Prelude

import Data.Either (Either)
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Prim.RowList as RL
import Type.Row as Row
import Yoga.Om (Om, fromAff, throw, throwLeftAsM)



-- | Turn an Aff of Either left right and an error constructor into an Om of right and an error record.
--  This is useful for turning Affs of Either into Om.
--  example usage:
--    fromAffThrowLeftAsOm (\err -> { oopsie: err }) (Aff (Left "Oops, something went wrong!")) -- > Om ctx { oopsie: String } String
--    fromAffThrowLeftAsOm (\err -> { oopsie: err }) (Aff (Right "You're beautiful")) -- > Om ctx { oopsie: String } String
fromAffThrowLeftAsOm
  :: forall right left err ctx errors key a otherErrors
  . RL.RowToList err (RL.Cons key a RL.Nil)
  => Row.Cons key a () err
  => Row.Cons key a otherErrors ( exception :: Error | errors)
  => IsSymbol key
  => (left -> Record err)
  -> Aff (Either left right)
  -> Om ctx errors right
fromAffThrowLeftAsOm toErr = (_ >>= throwLeftAsM (throw <<< toErr)) <<< fromAff
