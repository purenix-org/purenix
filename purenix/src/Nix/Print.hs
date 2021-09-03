{-# LANGUAGE OverloadedStrings #-}

module Nix.Print (renderExpr) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bool (bool)
import Data.List (intersperse)
import Data.Semigroup (mtimesDefault)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Nix.Expr hiding (string)

newtype PrintContext = PrintContext {pcIndent :: Int}

newtype PrintState = PrintState {psBuilder :: Builder}

newtype Printer = Printer {_unPrinter :: ReaderT PrintContext (State PrintState) ()}

runPrinter :: Printer -> TL.Text
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

renderExpr :: Expr -> TL.Text
renderExpr = runPrinter . fst3 . foldExpr render
  where
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    render :: ExprF (Printer, Style, Int) -> (Printer, Style, Int)
    render expr = (ppExpr sty parenthesized, sty, exprPrec expr)
      where
        sty = exprStyle (snd3 <$> expr)
        parenthesized = let f = parenthesize expr in (\(p, sty', prec) -> parens (f prec) sty' p) <$> expr

data Style = Single | Multi deriving (Eq, Ord)

style :: r -> r -> Style -> r
style a _ Single = a
style _ b Multi = b

exprStyle :: ExprF Style -> Style
exprStyle a@Attrs {} | length a > 1 = Multi
exprStyle Let {} = Multi
exprStyle v = bool Single Multi $ elem Multi v

exprPrec :: ExprF a -> Int
exprPrec Var {} = 10
exprPrec Num {} = 10
exprPrec String {} = 10
exprPrec Attrs {} = 10
exprPrec List {} = 10
exprPrec Sel {} = 9
exprPrec App {} = 8
exprPrec (Bin op _ _) = opPrec op
  where
    opPrec :: Op -> Int
    opPrec Update = 5
exprPrec Abs {} = 1
exprPrec Let {} = 0

parens :: Bool -> Style -> Printer -> Printer
parens False _ = id
parens True sty = delimit sty '(' ')'

parenthesize :: ExprF a -> Int -> Bool
parenthesize Attrs {} = const False
parenthesize Let {} = const False
parenthesize List {} = (< 10)
parenthesize e = (< exprPrec e)

sepBy :: Printer -> [Printer] -> Printer
sepBy sep = mconcat . intersperse sep

quotes :: Printer -> Printer
quotes p = char '"' <> p <> char '"'

binding :: (Ident, Printer) -> Printer
binding (ident, body) = text ident <> " = " <> body <> ";"

ppExpr :: Style -> ExprF Printer -> Printer
ppExpr _ (Var i) = text i
ppExpr _ (Abs arg body) = text arg <> ": " <> body
ppExpr _ (App f x) = f <> space <> x
ppExpr _ (Attrs [] [] []) = "{ }"
ppExpr sty (Attrs ih ihf b) = delimit sty '{' '}' $ sepBy newline $ inherits <> inheritFroms <> binds
  where
    inherits = ["inherit " <> sepBy space (text <$> ih) <> ";" | not (null ih)]
    inheritFroms = (\(from, idents) -> "inherit " <> delimit Single '(' ')' from <> space <> sepBy space (text <$> idents)) <$> ihf
    binds = (\(ident, body) -> text ident <> " = " <> body <> ";") <$> b
ppExpr _ (List []) = "[]"
ppExpr sty (List l) = delimit sty '[' ']' $ sepBy newline l
ppExpr _ (Sel a b) = a <> "." <> quotes (text b)
ppExpr _ (String str) = text str
ppExpr _ (Num n) = string (show n)
ppExpr _ (Let binds body) =
  mconcat
    [ "let",
      indent $ newline <> sepBy newline (binding <$> binds),
      newline,
      "in",
      indent $ newline <> body
    ]
ppExpr _ (Bin Update l r) = l <> " // " <> r
