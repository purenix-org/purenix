module Data.Semigroup.First where

import Control.Apply (class Apply)
import Control.Applicative (class Applicative)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Data.Bounded (class Bounded)
import Data.Eq (class Eq1, class Eq)
import Data.Functor (class Functor)
import Data.Ord (class Ord1, class Ord)
import Data.Semigroup (class Semigroup, (<>))
import Data.Show (class Show, show)

-- | Semigroup where `append` always takes the first option.
-- |
-- | ``` purescript
-- | First x <> First y == First x
-- | ```
newtype First a = First a

derive newtype instance eqFirst :: Eq a => Eq (First a)
derive instance eq1First :: Eq1 First

derive newtype instance ordFirst :: Ord a => Ord (First a)
derive instance ord1First :: Ord1 First

derive newtype instance boundedFirst :: Bounded a => Bounded (First a)

instance showFirst :: Show a => Show (First a) where
  show (First a) = "(First " <> show a <> ")"

derive instance functorFirst :: Functor First

instance applyFirst :: Apply First where
  apply (First f) (First x) = First (f x)

instance applicativeFirst :: Applicative First where
  pure = First

instance bindFirst :: Bind First where
  bind (First x) f = f x

instance monadFirst :: Monad First

instance semigroupFirst :: Semigroup (First a) where
  append x _ = x
