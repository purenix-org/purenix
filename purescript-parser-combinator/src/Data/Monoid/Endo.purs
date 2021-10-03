module Data.Monoid.Endo where

import Control.Category (class Category)
import Control.Semigroupoid (class Semigroupoid, (<<<))
import Data.Bounded (class Bounded)
import Data.Eq (class Eq)
import Data.Function (identity)
import Data.Monoid (class Monoid)
import Data.Ord (class Ord)
import Data.Semigroup (class Semigroup, (<>))
import Data.Show (class Show, show)

-- | Monoid and semigroup for category endomorphisms.
-- |
-- | When `c` is instantiated with `->` this composes functions of type
-- | `a -> a`:
-- |
-- | ``` purescript
-- | Endo f <> Endo g == Endo (f <<< g)
-- | (mempty :: Endo _) == Endo identity
-- | ```
newtype Endo :: forall k. (k -> k -> Type) -> k -> Type
newtype Endo c a = Endo (c a a)

derive newtype instance eqEndo :: Eq (c a a) => Eq (Endo c a)

derive newtype instance ordEndo :: Ord (c a a) => Ord (Endo c a)

derive newtype instance boundedEndo :: Bounded (c a a) => Bounded (Endo c a)

instance showEndo :: Show (c a a) => Show (Endo c a) where
  show (Endo x) = "(Endo " <> show x <> ")"

instance semigroupEndo :: Semigroupoid c => Semigroup (Endo c a) where
  append (Endo a) (Endo b) = Endo (a <<< b)

instance monoidEndo :: Category c => Monoid (Endo c a) where
  mempty = Endo identity
