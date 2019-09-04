# purescript-son-of-a-j

Zero cost json serialization / deserialization for a subset of PureScript types.

## Supported types

This library exploits internal JavaScript representations of some PureScript types to allow simple serialization and deserialization of their values.
It provides simple `class SonJ a` which proofs serializiblity of a given type.

Currently supported types are: `Number`, `String`, `Int`, `Boolean`, `Record`, `Variant`, `Null` (defined internally) and `newtype` (with `Newtype` instance) around any serializable type.

Users are not able to extend instance set as original instance chain is closed by fully polymorphic instance for the `newtype`... It is probably probably possible to implement your own `class` and instance chain with `SonJ` fallback though.

## Usage

### API

```purescript
dump ∷ ∀ a. SonJ a ⇒ a → Json

unsafeLoad ∷ ∀ a. SonJ a ⇒ Json → a
```

### Example usage

This is excerpt of `test/Main.purs`:

```purescript
roundTrip ∷ ∀ a. SonJ a ⇒ a → Either String a
roundTrip = SonJ.dump >>> stringify >>> jsonParser >>> map SonJ.unsafeLoad

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
  logShow (roundTrip (just 8) == Right (just 8))
  logShow (roundTrip (just 9) == Right (just 8))
  logShow (roundTrip (X {a: 8, b: "test"}) == Right (X {a: 8, b: "test"}))
```

## Limitations
As I'm in hurry it doesn't support any form of validation and provides only `unsafeLoad` as deserialization method.

