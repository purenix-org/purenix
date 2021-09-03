{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nix.Expr where

import Data.Map (Map)
import Data.Text (Text)

type Ident = Text

newtype Expr = Expr {unExpr :: ExprF Expr}
  deriving newtype (Show)

data ExprF f
  = Var Ident
  | Abs Ident f
  | App f f
  | Attrs (Map Ident f)
  | Sel f Ident
  | Let (Map Ident f) f
  | Num Integer
  | String Text
  deriving stock (Functor, Foldable, Traversable, Show)

foldExpr :: (ExprF r -> r) -> Expr -> r
foldExpr f = go where go = f . fmap go . unExpr
