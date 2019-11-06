{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name = "son-of-a-j"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "generics-rep"
    , "prelude"
    , "nullable"
    , "typelevel"
    , "variant"
    ]
, packages = ../magusai/packages.dhall
}

