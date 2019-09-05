# purescript-son-of-a-j

Zero cost json serialization and zero copy deserialization for a subset of PureScript types.

## Goals and design

- [x] No extensibilty. No additional instances required. Works out of the box.

- [x] Provide __efficient__ serialization for types which are internally represented as valid JSON.

- [ ] Provide __efficient__ transformations between any other generic PureScript type (think `Sum`) and it's json serializable version.

## Supported types

This library exploits internal JavaScript representations of some PureScript types to allow simple serialization and deserialization of their values.
It provides simple `class SonJ a` which proofs serializiblity of a given type.

Currently supported types are: `Number`, `String`, `Int`, `Boolean`, `Record`, `Variant`, `Array`, `Null` (defined internally) and `newtype` (with `Newtype` instance) around any serializable type.

To use this lib user don't have to provide any instances at all (check `X` type in example below).

## Limitations

This libary is not extensible by design as we want to support any `newtype` serialization out of the box. In other words our instance chain is closed by fully polymorphic instance for `Newtype` case.

## Usage

### API

```purescript

-- | Nearly zero cost

dump ∷ ∀ a. SonJ a ⇒ a → Json

unsafeLoad ∷ ∀ a. SonJ a ⇒ Json → a

-- | Zero copy but with validation (json traversing) cost

load ∷ ∀ a. SonJ a ⇒ Json → Maybe a
```

### Example usage

This is excerpt of `test/Main.purs`:

```purescript
unsafeRoundTrip ∷ ∀ a. SonJ a ⇒ a → Maybe a
unsafeRoundTrip = SonJ.dump >>> stringify >>> jsonParser >>> hush >>> map SonJ.unsafeLoad

roundTrip ∷ ∀ a. SonJ a ⇒ a → Maybe a
roundTrip = SonJ.dump >>> stringify >>> jsonParser >>> hush >=> SonJ.load

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


  logShow (roundTrip (just 8) == Just (just 8))
  logShow (roundTrip (just 9) /= Just (just 8))
  logShow (roundTrip (X {a: 8, b: just "test"}) == Just (X {a: 8, b: just "test"}))
  logShow (roundTrip (X {a: 8, b: just "test"}) /= Just (X {a: 8, b: nothing }))
  logShow (roundTrip (X {a: 8, b: nothing ∷ MaybeV Int}) == Just (X {a: 8, b: nothing }))
```

