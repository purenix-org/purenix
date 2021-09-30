module Test.Main where

import Prelude
import Effect (Effect)
import Test.Assert (assert)
import Safe.Coerce (coerce)

newtype X = X String

main :: Effect Unit
main =
  assert $ "hi" == coerce (X "hi")

