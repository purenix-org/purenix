{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Nix.Expr where

import Data.List.NonEmpty (NonEmpty (..))
import Nix.Identifiers
import Nix.Prelude

newtype Expr = Expr {unExpr :: ExprF Expr}
  deriving newtype (Show)

data ExprF f
  = Var Var
  | Lam Var f
  | App f f
  | Attrs [Var] [(f, [Key])] [(Key, f)]
  | Cond f f f
  | List [f]
  | Bin Op f f
  | Not f
  | Sel f Key
  | Let (NonEmpty (Var, f)) f
  | Int Integer
  | Double Double
  | String Text
  | Path Text
  deriving stock (Functor, Foldable, Traversable, Show)

data Op = Update | Equals | And
  deriving (Eq, Show)

foldExpr :: (ExprF r -> r) -> Expr -> r
foldExpr f = go where go = f . fmap go . unExpr

var :: Var -> Expr
var = Expr . Var

lam :: Var -> Expr -> Expr
lam arg body = Expr $ Lam arg body

app :: Expr -> Expr -> Expr
app f x = Expr $ App f x

cond :: Expr -> Expr -> Expr -> Expr
cond c true false = Expr $ Cond c true false

attrs ::
  [Var] ->
  [(Expr, [Key])] ->
  [(Key, Expr)] ->
  Expr
attrs inherits inheritFroms binds = Expr $ Attrs inherits inheritFroms binds

sel :: Expr -> Key -> Expr
sel e s = Expr $ Sel e s

let' :: [(Var, Expr)] -> Expr -> Expr
let' [] body = body
let' (h : t) body = Expr $ Let (h :| t) body

int :: Integer -> Expr
int = Expr . Int

double :: Double -> Expr
double = Expr . Double

string :: Text -> Expr
string = Expr . String

list :: [Expr] -> Expr
list = Expr . List

bin :: Op -> Expr -> Expr -> Expr
bin op a b = Expr $ Bin op a b

path :: Text -> Expr
path = Expr . Path

constructorFieldNames :: [Var]
constructorFieldNames = numberedVars "__field"

not' :: Expr -> Expr
not' = Expr . Not

builtin :: Key -> Expr
builtin = sel (var "builtins")

--   Just
-- becomes
--   (a: { __tag = "Just"; __field0 = a; })
constructor :: Text -> [Var] -> Expr
constructor conName fields =
  foldr
    lam
    ( attrs
        []
        []
        (("__tag", string conName) : zipWith (\arg (UnsafeVar name) -> (UnsafeKey name, var arg)) fields constructorFieldNames)
    )
    fields
