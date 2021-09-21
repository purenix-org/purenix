module Parser.Parsec
  -- ( Parser,
  --   runParser,
  --   token,
  --   throwAt,
  -- )
where

import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Ord (compare)
import Data.Ordering (Ordering(..))
import Data.Semigroup (class Semigroup, append)
import Data.Tuple.Nested ((/\), type (/\))

data Err e = Err Int e

instance semigroupErr :: Semigroup e => Semigroup (Err e) where
  append (Err il el) (Err ir er) = case compare il ir of
    LT -> Err ir er
    EQ -> Err ir (append el er)
    GT -> Err il el

instance monoidErr :: Monoid e => Monoid (Err e) where
  mempty = Err 0 mempty

-- | A simple backtracking parser.
-- Maintains the error message of whatever branch managed to consume the most input.
newtype Parser e a = Parser
  ( forall b.
    String ->
    Int -> -- Offset
    Err e -> -- error set
    (a -> Int -> Err e -> b) -> -- Success continuation
    (Err e -> b) -> -- Error continuation
    b
  )

unParser
  :: forall b e a
   . Parser e a -> 
     String ->
     Int -> -- Offset
     Err e -> -- error set
     (a -> Int -> Err e -> b) -> -- Success continuation
     (Err e -> b) -> -- Error continuation
     b
unParser (Parser p) = p

runParser ::
  forall e a.
  Monoid e =>
  String ->
  Parser e a ->
  Either (Int /\ e) a
runParser s (Parser p) = p s 0 mempty (\a _ _ -> Right a) (\(Err i e) -> Left (i /\ e))

-- instance Functor (Parser t e) where
--   {-# INLINE fmap #-}
--   fmap f (Parser k) = Parser $ \t i e ok -> k t i e (ok . f)

-- instance Applicative (Parser t e) where
--   {-# INLINE pure #-}
--   pure a = Parser $ \_ i e ok _ -> ok a i e
--   {-# INLINE (<*>) #-}
--   Parser pf <*> Parser pa = Parser $ \t i e ok ng -> pf t i e (\f i' e' -> pa t i' e' (ok . f) ng) ng

-- instance Monad (Parser t e) where
--   {-# INLINE (>>=) #-}
--   Parser k >>= f = Parser $ \t i e ok ng -> k t i e (\a i' e' -> unParser (f a) t i' e' ok ng) ng

-- instance Monoid e => Alternative (Parser t e) where
--   {-# INLINE empty #-}
--   empty = Parser $ \_ _ e _ ng -> ng e
--   {-# INLINE (<|>) #-}
--   Parser pl <|> Parser pr = Parser $ \t i e ok ng ->
--     pl t i e ok $ \e' ->
--       pr t i e' ok ng

-- instance Monoid e => MonadPlus (Parser t e)

-- {-# INLINE token #-}
-- token :: Parser t e t
-- token = Parser $ \t i e ok _ -> ok (t i) (i + 1) e

-- {-# INLINE throwAt #-}
-- throwAt :: Semigroup e => ((forall err. e -> Parser t e err) -> Parser t e a) -> Parser t e a
-- throwAt k = Parser $ \t i e ok err ->
--   let throw' e = Parser $ \_ _ e' _ err' -> err' (e' <> Err i e)
--    in unParser (k throw') t i e ok err
