module Data.Monoid.Additive where

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
import Data.Semiring (class Semiring, (+), zero)
import Data.Show (class Show, show)

-- | Monoid and semigroup for semirings under addition.
-- |
-- | ``` purescript
-- | Additive x <> Additive y == Additive (x + y)
-- | (mempty :: Additive _) == Additive zero
-- | ```
newtype Additive a = Additive a

derive newtype instance eqAdditive :: Eq a => Eq (Additive a)
derive instance eq1Additive :: Eq1 Additive

derive newtype instance ordAdditive :: Ord a => Ord (Additive a)
derive instance ord1Additive :: Ord1 Additive

derive newtype instance boundedAdditive :: Bounded a => Bounded (Additive a)

instance showAdditive :: Show a => Show (Additive a) where
  show (Additive a) = "(Additive " <> show a <> ")"

derive instance functorAdditive :: Functor Additive

instance applyAdditive :: Apply Additive where
  apply (Additive f) (Additive x) = Additive (f x)

instance applicativeAdditive :: Applicative Additive where
  pure = Additive

instance bindAdditive :: Bind Additive where
  bind (Additive x) f = f x

instance monadAdditive :: Monad Additive

instance semigroupAdditive :: Semiring a => Semigroup (Additive a) where
  append (Additive a) (Additive b) = Additive (a + b)

instance monoidAdditive :: Semiring a => Monoid (Additive a) where
  mempty = Additive zero
