-- | This module is **deprecated** and will be removed in a future release.
-- |
-- | The annihilation law witnessed by `MonadZero` is trivially satisfied by
-- | lawful monads due to parametricity: while evaluating `empty >>= f`, the
-- | function `f` can’t ever be called, since that would require `empty` to
-- | produce a value, which means that `empty >>= f` must be the same as
-- | `empty >>= pure`, which by the monad laws is just `empty`.
-- |
-- | Use `Monad` and `Alternative` constraints instead.

module Control.MonadZero
  ( class MonadZeroIsDeprecated
  , class MonadZero
  , module Control.Alt
  , module Control.Alternative
  , module Control.Applicative
  , module Control.Apply
  , module Control.Bind
  , module Control.Monad
  , module Control.Plus
  , module Data.Functor
  ) where

import Control.Alt (class Alt, alt, (<|>))
import Control.Alternative (class Alternative, guard)
import Control.Applicative (class Applicative, pure, liftA1, unless, when)
import Control.Apply (class Apply, apply, (*>), (<*), (<*>))
import Control.Bind (class Bind, bind, ifM, join, (<=<), (=<<), (>=>), (>>=))
import Control.Monad (class Monad, ap, liftM1)
import Control.Plus (class Plus, empty)

import Data.Functor (class Functor, map, void, ($>), (<#>), (<$), (<$>))

import Prim.TypeError (class Warn, Text)

class MonadZeroIsDeprecated
instance monadZeroIsDeprecated :: Warn (Text "'MonadZero' is deprecated, use 'Monad' and 'Alternative' constraints instead") => MonadZeroIsDeprecated

-- | The `MonadZero` type class has no members of its own; it just specifies
-- | that the type has both `Monad` and `Alternative` instances.
-- |
-- | Types which have `MonadZero` instances should also satisfy the following
-- | laws:
-- |
-- | - Annihilation: `empty >>= f = empty`
class (Monad m, Alternative m, MonadZeroIsDeprecated) <= MonadZero m

instance monadZeroArray :: MonadZero Array
