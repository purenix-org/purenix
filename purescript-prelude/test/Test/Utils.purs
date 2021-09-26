module Test.Utils where

import Prelude

type AlmostEff = Unit -> Unit

assert :: String -> Boolean -> AlmostEff
assert msg condition = if condition then const unit else throwErr msg

foreign import throwErr :: String -> AlmostEff
