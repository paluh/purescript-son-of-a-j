# purescript-son-of-a-j

Zero cost json serialization / deserialization for subset of PureScript types.

## Supported types

This library exploits internal representations of some PureScript types to allow simple serialization and deserialization of their values.
It provides simple `class SonJ a` which proofs serializiblity of a given type.

Currently supported types are: `Number`, `String`, `Int`, `Boolean`, `Record`, `Variant` and newtypes around previous subset.

## Limitations
As I'm in hurry it doesn't support any form of validation and provides only `unsafeLoad` as deserialization method.

