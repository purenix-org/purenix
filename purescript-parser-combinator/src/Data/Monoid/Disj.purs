module Data.Monoid.Disj where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Data.Bounded (class Bounded)
import Data.Eq (class Eq1, class Eq)
import Data.Functor (class Functor)
import Data.HeytingAlgebra (class HeytingAlgebra, disj, conj, ff, tt)
import Data.Monoid (class Monoid)
import Data.Ord (class Ord1, class Ord)
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring (class Semiring)
import Data.Show (class Show, show)

-- | Monoid and semigroup for disjunction.
-- |
-- | ``` purescript
-- | Disj x <> Disj y == Disj (x || y)
-- | (mempty :: Disj _) == Disj bottom
-- | ```
newtype Disj a = Disj a

derive newtype instance eqDisj :: Eq a => Eq (Disj a)
derive instance eq1Disj :: Eq1 Disj

derive newtype instance ordDisj :: Ord a => Ord (Disj a)
derive instance ord1Disj :: Ord1 Disj

derive newtype instance boundedDisj :: Bounded a => Bounded (Disj a)

instance showDisj :: Show a => Show (Disj a) where
  show (Disj a) = "(Disj " <> show a <> ")"

derive instance functorDisj :: Functor Disj

instance applyDisj :: Apply Disj where
  apply (Disj f) (Disj x) = Disj (f x)

instance applicativeDisj :: Applicative Disj where
  pure = Disj

instance bindDisj :: Bind Disj where
  bind (Disj x) f = f x

instance monadDisj :: Monad Disj

instance semigroupDisj :: HeytingAlgebra a => Semigroup (Disj a) where
  append (Disj a) (Disj b) = Disj (disj a b)

instance monoidDisj :: HeytingAlgebra a => Monoid (Disj a) where
  mempty = Disj ff

instance semiringDisj :: HeytingAlgebra a => Semiring (Disj a) where
  zero = Disj ff
  one = Disj tt
  add (Disj a) (Disj b) = Disj (disj a b)
  mul (Disj a) (Disj b) = Disj (conj a b)
