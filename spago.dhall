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
, license = "BSD-3-Clause"
, repository = "https://github.com/paluh/son-of-a-j.git"
, packages = ./packages.dhall
}
