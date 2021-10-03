module Test.Main where

import Effect (Effect)
import Effect.Console (log)

import Data.Functor (class Functor)
import Data.Semigroup ((<>))
import Data.Unit (Unit)

import Unsafe.Coerce (unsafeCoerce)

newtype Foo = Foo String

newtype Bar = Bar String

-- | The two newtypes `Foo` and `Bar` have the same runtime representation,
-- | so it is safe to coerce one into the other directly.
coerceFoo :: Foo -> Bar
coerceFoo = unsafeCoerce

-- | It is also safe to coerce entire collections, without having to map over
-- | individual elements.
coerceFoos :: forall f. Functor f => f Foo -> f Bar
coerceFoos = unsafeCoerce

main :: Effect Unit
main =
  case coerceFoos [Foo "Hello", Foo " ", Foo "World"] of
    [Bar x, Bar y, Bar z] -> log (x <> y <> z)
    _ -> log "impossible"
