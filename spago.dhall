{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "son-of-a-j"
, dependencies =
  [ "argonaut"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foreign-object"
  , "integers"
  , "js-unsafe-stringify"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "record"
  , "typelevel"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
}
