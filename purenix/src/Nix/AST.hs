{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nix.AST where

import Data.Map (Map)
import Data.Text (Text)

type Ident = Text

newtype Nix = Nix (NixF Nix)
  deriving newtype (Show)

data NixF f
  = Var Ident
  | Num Integer
  | Lam Ident f
  | App f f
  | Attrs (Map Ident f)
  | Sel f Ident
  | Let (Map Ident f) f
  deriving stock (Functor, Foldable, Traversable, Show)
