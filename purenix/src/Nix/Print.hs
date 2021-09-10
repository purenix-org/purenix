{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Nix.Print (renderExpr) where

import Data.Char (isAlphaNum)
import Data.List (intersperse)
import Data.Semigroup (mtimesDefault)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Lens.Micro.Platform
import Nix.Expr hiding (string)
import Nix.Prelude
import Nix.Util (nixKeywords)

newtype PrintContext = PrintContext {pcIndent :: Int}

newtype PrintState = PrintState {psBuilder :: Builder}

newtype Printer = Printer {_unPrinter :: ReaderT PrintContext (State PrintState) ()}

runPrinter :: Printer -> LText
runPrinter (Printer p) = TB.toLazyText $ psBuilder $ execState (runReaderT p pc0) ps0
  where
    pc0 = PrintContext 0
    ps0 = PrintState mempty

instance Semigroup Printer where Printer a <> Printer b = Printer (a >> b)

instance Monoid Printer where mempty = Printer (pure ())

instance IsString Printer where fromString = Printer . emit . fromString

delimit :: Style -> Char -> Char -> Printer -> Printer
delimit = style delimitSingle delimitMulti
  where
    delimitSingle :: Char -> Char -> Printer -> Printer
    delimitSingle open close body = mconcat [char open, body, char close]
    delimitMulti :: Char -> Char -> Printer -> Printer
    delimitMulti open close body = mconcat [newline, char open, space, indent body, newline, char close]

space :: Printer
space = char ' '

indent :: Printer -> Printer
indent (Printer p) = Printer $ local (\(PrintContext n) -> PrintContext (n + 2)) p

char :: Char -> Printer
char = Printer . emit . TB.singleton

emit :: Builder -> ReaderT PrintContext (State PrintState) ()
emit t = modify (\(PrintState s) -> PrintState $ s <> t)

text :: Text -> Printer
text = Printer . emit . TB.fromText

string :: String -> Printer
string = Printer . emit . TB.fromString

newline :: Printer
newline = Printer $ do
  i <- asks pcIndent
  emit ("\n" <> mtimesDefault i " ")

renderExpr :: Expr -> LText
renderExpr = runPrinter . view _1 . foldExpr render
  where
    render :: ExprF (Printer, Style, Associativity, Precedence) -> (Printer, Style, Associativity, Precedence)
    render expr = (ppExpr sty parenthesized, sty, exprAssoc expr, exprPrec expr)
      where
        sty = exprStyle (view _2 <$> expr)
        parenthesized =
          parenthesize
            (view _3)
            (view _4)
            (view _1)
            (\inner -> delimit (inner ^. _2) '(' ')' (inner ^. _1))
            expr

data Style = Single | Multi deriving (Eq, Ord)

style :: r -> r -> Style -> r
style a _ Single = a
style _ b Multi = b

exprStyle :: ExprF Style -> Style
exprStyle (Attrs [_] [] []) = Single
exprStyle (Attrs [] [(sty, _)] []) = sty
exprStyle (Attrs [] [] [(_, sty)]) = sty
exprStyle Attrs {} = Multi
exprStyle Let {} = Multi
exprStyle v = bool Single Multi $ elem Multi v

newtype Precedence = Precedence Int deriving newtype (Num, Eq, Ord)

data Associativity = AssocLeft | AssocRight | AssocNone
  deriving (Eq, Show)

exprAssoc :: ExprF a -> Associativity
exprAssoc Sel {} = AssocLeft
exprAssoc App {} = AssocLeft
exprAssoc (Bin op _ _) = opAssoc op
  where
    opAssoc Update = AssocRight
    opAssoc Equals = AssocNone
    opAssoc And = AssocLeft
exprAssoc _ = AssocNone

-- | Expression precedence.
-- See: https://nixos.org/manual/nix/stable/#sec-language-operators
-- Opersators listed in the above table have a precedence of (15 - <listed precedence>)
exprPrec :: ExprF a -> Precedence
exprPrec Var {} = 15
exprPrec Int {} = 15
exprPrec Double {} = 15
exprPrec String {} = 15
exprPrec Attrs {} = 15
exprPrec List {} = 15
exprPrec Sel {} = 14
exprPrec App {} = 13
exprPrec Has {} = 11
exprPrec Not {} = 8
exprPrec (Bin op _ _) = opPrec op
  where
    opPrec :: Op -> Precedence
    opPrec Update = 6
    opPrec Equals = 4
    opPrec And = 3
exprPrec Cond {} = 1
exprPrec Lam {} = 0
exprPrec Let {} = 0

parenthesize :: forall a b. (a -> Associativity) -> (a -> Precedence) -> (a -> b) -> (a -> b) -> ExprF a -> ExprF b
parenthesize assoc prec no yes = go
  where
    below :: Precedence -> a -> b
    below p a = if prec a < p then yes a else no a
    bin :: (forall c. c -> c -> ExprF c) -> a -> a -> ExprF b
    bin op l r = op (f l AssocLeft) (f r AssocRight)
      where
        f x a = case compare (prec x) (exprPrec $ op () ()) of
          LT -> no x
          EQ | assoc x == a -> no x
          _ -> yes x
    go :: ExprF a -> ExprF b
    go (Attrs ih ihf f) = Attrs ih (ihf & traverse . _1 %~ yes) (f & traverse . _2 %~ no)
    go (Let binds body) = Let (binds & traverse . _2 %~ no) (body & no)
    go (List elems) = List (below 14 <$> elems)
    go (App f x) = bin App f x
    go (Bin op l r) = bin (Bin op) l r
    go e = fmap (below (exprPrec e)) e

sepBy :: Printer -> [Printer] -> Printer
sepBy sep = mconcat . intersperse sep

quotes :: Printer -> Printer
quotes p = char '"' <> p <> char '"'

binding :: (Ident, Printer) -> Printer
binding (ident, body) = escape ident <> " = " <> indent body <> ";"

escape :: Text -> Printer
escape t =
  if T.all (\c -> isAlphaNum c || c == '_') t && (t `notElem` nixKeywords)
    then text t
    else quotes (text t)

ppExpr :: Style -> ExprF Printer -> Printer
ppExpr _ (Var i) = text i
ppExpr _ (Lam arg body) = text arg <> ": " <> body
ppExpr _ (App f x) = f <> space <> x
ppExpr _ (Attrs [] [] []) = "{ }"
ppExpr sty (Attrs ih ihf b) = delimit sty '{' '}' $ sepBy newline $ inherits <> inheritFroms <> binds
  where
    inherits = [sepBy space ("inherit" : (text <$> ih)) <> ";" | not (null ih)]
    inheritFroms = (\(from, idents) -> sepBy space ("inherit" : from : (text <$> idents)) <> ";") <$> ihf
    binds = binding <$> b
ppExpr _ (List []) = "[]"
ppExpr sty (List l) = delimit sty '[' ']' $ sepBy newline l
ppExpr _ (Sel a b) = a <> "." <> escape b
ppExpr _ (Path t) = text t
ppExpr _ (String str) = char '"' <> text str <> char '"'
ppExpr _ (Int n) = string (show n)
ppExpr _ (Double x) = string (show x)
ppExpr Single (Cond c t f) = sepBy space ["if", c, "then", t, "else", f]
ppExpr Multi (Cond c t f) = newline <> "if " <> c <> indent (newline <> "then " <> t <> "else " <> f)
ppExpr _ (Has l r) = l <> " ? " <> text r
ppExpr _ (Not e) = "!" <> e
ppExpr _ (Let binds body) =
  mconcat
    [ newline,
      "let",
      indent $ newline <> sepBy newline (binding <$> binds),
      newline,
      "in",
      indent body
    ]
ppExpr _ (Bin Update l r) = l <> " // " <> r
ppExpr _ (Bin Equals l r) = l <> " == " <> r
ppExpr _ (Bin And l r) = l <> " && " <> r
