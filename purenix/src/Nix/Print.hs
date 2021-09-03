{-# LANGUAGE OverloadedStrings #-}

module Nix.Print (renderNix) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Semigroup (mtimesDefault)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Nix.Expr (Expr)
import Text.Pretty.Simple (pShowNoColor)

newtype PrintContext = PrintContext {pcIndent :: Int}

newtype PrintState = PrintState {_builder :: Builder}

newtype Printer = Printer {unPrinter :: ReaderT PrintContext (State PrintState) ()}

instance Semigroup Printer where
  Printer a <> Printer b = Printer (a >> b)

instance Monoid Printer where
  mempty = Printer (pure ())

delimit :: Char -> Char -> Printer -> Printer
delimit open close body = mconcat [newline, char open, space, indent body, newline, char close]

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

newline :: Printer
newline = Printer $ do
  i <- asks pcIndent
  emit ("\n" <> mtimesDefault i " ")

renderNix :: Expr -> TL.Text
renderNix = pShowNoColor
