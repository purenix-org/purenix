module Tests where

data TestCase
data Result

foreign import success :: String -> TestCase
foreign import failure :: String -> TestCase
foreign import run :: Array TestCase -> Result

suite :: Result
suite = run
  [ success "trivial"
  , success "String literals with a \" don't break things"
  , success """multi-line
"strings"
don't break things
"""
  , (case "\"" of
       "\"" -> success
       _-> failure
    ) "Pattern matching on a \" works"
  ]
