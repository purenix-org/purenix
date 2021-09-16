module Main where


-----------------------------------
---- Functor, Applicative, Monad --
-----------------------------------

--class Functor f where
--  map :: forall a b. (a -> b) -> f a -> f b

--infixl 4 map as <$>

--instance functorFn :: Functor ((->) r) where
--  map = compose

--void :: forall f a. Functor f => f a -> f Unit
--void = map (const unit)

---- | Ignore the return value of a computation, using the specified return value
---- | instead.
--voidRight :: forall f a b. Functor f => a -> f b -> f a
--voidRight x = map (const x)

--infixl 4 voidRight as <$

---- | A version of `voidRight` with its arguments flipped.
--voidLeft :: forall f a b. Functor f => f a -> b -> f b
--voidLeft f x = const x <$> f

--infixl 4 voidLeft as $>

-------------
---- Tuple --
-------------

--data Tuple a b = Tuple a b

--infixr 6 Tuple as /\
--infixr 6 type Tuple as /\

--derive instance functorTuple :: Functor (Tuple a)

--------------
---- StateT --
--------------

--newtype StateT s m a = StateT (s -> m (Tuple a s))

--runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
--runStateT (StateT s) = s

---- | Run a computation in the `StateT` monad, discarding the final state.
--evalStateT :: forall s m a. Functor m => StateT s m a -> s -> m a
--evalStateT (StateT m) s = fst <$> m s

---- | Run a computation in the `StateT` monad discarding the result.
--execStateT :: forall s m a. Functor m => StateT s m a -> s -> m s
--execStateT (StateT m) s = snd <$> m s

---- | Change the result type in a `StateT` monad action.
--mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
--mapStateT f (StateT m) = StateT (f <<< m)


---- type Parser a = ExceptT ParseError (State String) a
