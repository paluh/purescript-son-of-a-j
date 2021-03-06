module Test.Main where

import Prelude

import Data.Argonaut (jsonParser, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Class.Console (logShow)
import SonJ (class SonJ, Null, null)
import SonJ (dump, load, unsafeLoad) as SonJ
import Test.SonJ.FromSonJ (suite) as Test.SonJ.FromSonJ
import Test.SonJ.NTuple (suite) as Test.SonJ.NTuple
import Test.Utils (refEq)
import Type.Data.Symbol (SProxy(..))

unsafeRoundTrip ∷ ∀ a. SonJ a ⇒ a → Maybe a
unsafeRoundTrip = SonJ.dump >>> stringify >>> jsonParser >>> hush >>> \parsed → do
  json ← parsed
  let
    a = SonJ.unsafeLoad json
  if a `refEq` json
    then pure a
    else Nothing

coerceTrip ∷ ∀ a b. SonJ a ⇒ SonJ b ⇒ b → Maybe a
coerceTrip = SonJ.dump >>> stringify >>> jsonParser >>> hush >=> SonJ.load

roundTrip ∷ ∀ a. SonJ a ⇒ a → Maybe a
roundTrip = SonJ.dump >>> stringify >>> jsonParser >>> hush >>> \parsed → do
  json ← parsed
  a ← SonJ.load json
  if a `refEq` json
    then pure a
    else Nothing

type MaybeV a = Variant (just ∷ a, nothing ∷ Null)

just ∷ ∀ a. a → MaybeV a
just = inj (SProxy ∷ SProxy "just")

nothing ∷ ∀ a. MaybeV a
nothing = inj (SProxy ∷ SProxy "nothing") null

newtype X a b = X { a ∷ a, b ∷ b }
derive instance eqX ∷ (Eq a, Eq b) ⇒ Eq (X a b)
derive instance newtypeX ∷ Newtype (X a b) _

main ∷ Effect Unit
main = do
  logShow (unsafeRoundTrip (just 8) == Just (just 8))
  logShow (unsafeRoundTrip (just 9) /= Just (just 8))
  logShow (unsafeRoundTrip (X {a: 8, b: just "test"}) == Just (X {a: 8, b: just "test"}))
  logShow (unsafeRoundTrip (X {a: 8, b: just "test"}) /= Just (X {a: 8, b: nothing }))
  logShow (unsafeRoundTrip (X {a: 8, b: nothing ∷ MaybeV Int}) == Just (X {a: 8, b: nothing }))
  logShow $ eq
    (unsafeRoundTrip ([ X {a: 8, b: nothing }, X { a: 9, b: just 2 }]))
    (Just ([ X {a: 8, b: nothing }, X { a: 9, b: just 2 }]))

  logShow (roundTrip (just 8) == Just (just 8))
  logShow (roundTrip (just 9) /= Just (just 8))
  logShow (roundTrip (X {a: 8, b: just "test"}) == Just (X {a: 8, b: just "test"}))
  logShow (roundTrip (X {a: 8, b: just "test"}) /= Just (X {a: 10, b: nothing }))
  logShow (roundTrip (X {a: 8, b: nothing ∷ MaybeV Int}) == Just (X {a: 8, b: nothing }))
  logShow $ eq
    (roundTrip ([ X {a: 8, b: nothing }, X { a: 9, b: just 2 }]))
    (Just ([ X {a: 8, b: nothing }, X { a: 9, b: just 2 }]))
  logShow $ not $ eq
    (roundTrip ([ X {a: 8, b: nothing }, X { a: 9, b: just 8 }]))
    (Just ([ X {a: 8, b: nothing }, X { a: 9, b: just 2 }]))
  logShow $ (coerceTrip (X {a: 8, b: "test"}) ∷ (Maybe (X Int Int))) == Nothing
  logShow $ (coerceTrip (X {a: 8, b: "test"}) == Just { a: 8, b: "test" })

  Test.SonJ.FromSonJ.suite
  Test.SonJ.NTuple.suite
