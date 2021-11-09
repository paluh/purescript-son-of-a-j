module Test.SonJ.FromSonJ where

import Prelude
import Data.Argonaut (Json, jsonParser, stringify)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import SonJ (class SonJ, Null, null)
import SonJ (dump, load, unsafeLoad) as SonJ
import SonJ.FromSonJ (class FromSonJ)
import SonJ.FromSonJ (load) as SonJ.FromSonJ
import SonJ.FromSonJ.Builder (Builder, Unique(..), mapMaybe, unUnique)
import SonJ.FromSonJ.Builder (build, sonJ, string) as SonJ.FromSonJ.Builder
import Test.Utils (refEq)
import Type.Prelude (SProxy(..))

type MaybeV a
  = Variant ( just ∷ a, nothing ∷ Null )

just ∷ ∀ a. a → MaybeV a
just = inj (SProxy ∷ SProxy "just")

nothing ∷ ∀ a. MaybeV a
nothing = inj (SProxy ∷ SProxy "nothing") null

newtype X a b
  = X { a ∷ a, b ∷ b }

derive instance (Eq a, Eq b) ⇒ Eq (X a b)

derive instance Newtype (X a b) _

unsafeRoundTrip ∷ ∀ a. SonJ a ⇒ a → Maybe a
unsafeRoundTrip =
  SonJ.dump >>> stringify >>> jsonParser >>> hush
    >>> \parsed → do
        json ← parsed
        let
          a = SonJ.unsafeLoad json
        if a `refEq` json then
          pure a
        else
          Nothing

coerceTrip ∷ ∀ a b c. FromSonJ (Unique b) (Unique c) ⇒ SonJ b ⇒ SonJ a ⇒ a → (Maybe c)
coerceTrip = SonJ.dump >>> stringify >>> flip SonJ.FromSonJ.Builder.build (SonJ.FromSonJ.Builder.sonJ >>> mapMaybe (SonJ.FromSonJ.load ∷ Builder (Unique b) (Unique (c)))) >>> unUnique

newtype Wrap a
  = Wrap a

derive instance Eq a ⇒ Eq (Wrap a)

derive instance Generic (Wrap a) _

derive instance Newtype (Wrap a) _

suite ∷ Effect Unit
suite = do
  log "FromSonJ"
  logShow $ (coerceTrip ([ 8, 9 ]) ∷ (Maybe (Wrap (Array (Wrap Int))))) == Just (Wrap ([ Wrap 8, Wrap 9 ]))
  pure unit
