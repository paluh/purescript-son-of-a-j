module Test.SonJ.NTuple where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log, logShow)
import SonJ.NTuple (cons, tuple) as NTuple

suite ∷ Effect Unit
suite = do
  let
    x = NTuple.cons 8 (NTuple.tuple "a" 8.0)
    y = NTuple.cons 8 (NTuple.tuple "a" 9.0)
    x' = NTuple.cons 8 (NTuple.tuple "a" 8.0)

  log "NTuple"
  logShow $ x == x'
  logShow $ x == y
  pure unit
