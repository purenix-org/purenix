module Test.Main (main) where

import Effect (Effect)
import Data.Unit (Unit)

import Test.Control.Lazy (testLazy)

main :: Effect Unit
main = testLazy
