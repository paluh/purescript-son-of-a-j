let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c
in upstream
  with
    js-unsafe-stringify =
      { dependencies = ([] : List Text)
      , repo = "https://github.com/paluh/purescript-js-unsafe-stringify.git"
      , version = "v0.1.0"
      }
