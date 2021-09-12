module Main where

-- import Prelude

-- import Effect (Effect)
-- import Effect.Console (log)

-- main :: Effect Unit
-- main = do
--   log "🍝"

data FooBarBaz a
  = Foo
  | Bar Int
  | Baz a String Number

myCaseTest :: FooBarBaz Int -> Int
myCaseTest fbb =
  case fbb of
    Foo -> 999
    Bar i -> i
    Baz i "hello" _ -> i
    Baz _ _ _ -> 0
