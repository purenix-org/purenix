module Type.Data.Row where

-- | A proxy data type whose type parameter is a type of kind `Row Type` (a row
-- | of types).
-- |
-- | **Deprecated as of v0.14.0 PureScript release**: use `Type.Proxy` instead.
-- |
-- | Commonly used for specialising a function with a quantified type.
-- | For example, suppose we have an identity function for records of type:
-- | ```purescript
-- | recordIdentity :: forall row . RProxy row -> Record row -> Record row
-- | recordIdentity _ rec = rec
-- | ```
-- | Then applying this function to an `RProxy` with a specialised type
-- | allows us to specify a concrete type for `row`:
-- | ```purescript
-- | :t recordIdentity (Proxy :: Proxy ( x :: Int, y :: Int ))
-- | { x :: Int, y :: Int } -> { x :: Int, y :: Int }
-- | ```
-- | Here `row` has been specialised to `( x :: Int, y :: Int )`.
data RProxy :: Row Type -> Type
data RProxy row = RProxy
