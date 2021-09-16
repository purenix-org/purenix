module Data.Monoid.Multiplicative where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Data.Bounded (class Bounded)
import Data.Eq (class Eq1, class Eq)
import Data.Functor (class Functor)
import Data.Monoid (class Monoid)
import Data.Ord (class Ord1, class Ord)
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring (class Semiring, (*), one)
import Data.Show (class Show, show)

-- | Monoid and semigroup for semirings under multiplication.
-- |
-- | ``` purescript
-- | Multiplicative x <> Multiplicative y == Multiplicative (x * y)
-- | (mempty :: Multiplicative _) == Multiplicative one
-- | ```
newtype Multiplicative a = Multiplicative a

derive newtype instance eqMultiplicative :: Eq a => Eq (Multiplicative a)
derive instance eq1Multiplicative :: Eq1 Multiplicative

derive newtype instance ordMultiplicative :: Ord a => Ord (Multiplicative a)
derive instance ord1Multiplicative :: Ord1 Multiplicative

derive newtype instance boundedMultiplicative :: Bounded a => Bounded (Multiplicative a)

instance showMultiplicative :: Show a => Show (Multiplicative a) where
  show (Multiplicative a) = "(Multiplicative " <> show a <> ")"

derive instance functorMultiplicative :: Functor Multiplicative

instance applyMultiplicative :: Apply Multiplicative where
  apply (Multiplicative f) (Multiplicative x) = Multiplicative (f x)

instance applicativeMultiplicative :: Applicative Multiplicative where
  pure = Multiplicative

instance bindMultiplicative :: Bind Multiplicative where
  bind (Multiplicative x) f = f x

instance monadMultiplicative :: Monad Multiplicative

instance semigroupMultiplicative :: Semiring a => Semigroup (Multiplicative a) where
  append (Multiplicative a) (Multiplicative b) = Multiplicative (a * b)

instance monoidMultiplicative :: Semiring a => Monoid (Multiplicative a) where
  mempty = Multiplicative one
