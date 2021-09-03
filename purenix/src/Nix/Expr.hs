{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nix.Expr where

import Data.Map (Map)
import Data.Text (Text)

type Ident = Text

newtype Expr = Expr (ExprF Expr)
  deriving newtype (Show)

data ExprF f
  = Var Ident
  | Num Integer
  | Abs Ident f
  | App f f
  | Attrs (Map Ident f)
  | Sel f Ident
  | Let (Map Ident f) f
  deriving stock (Functor, Foldable, Traversable, Show)
