{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the types and conversions from PureScript/CoreFn identifiers to Nix identifiers.
-- This can be a tricky problem, since all three languages have different rules for what is and isn't allowed in certain kinds of identifiers.
-- The goal of this module is to provide an API that takes care of all those concerns.
-- In other words, the types in this module are as close to ready-to-print as possible.
--
-- The reasoning is that 'Key' and 'Var' are intended to be used in 'Expr'.
-- 'Expr' should represent a _valid_ Nix expression, and therefore 'Key' and 'Var' need to represent valid keys and binders, respectively.
-- We don't have to care about rejecting nonsensical values during printing.
--
-- Nix's rules for naked (non-quoted) identifiers are strictly more lenient than PureScripts (see links to specifications below), since nix allows '-' in identifiers (they cannot start with a '-' though).
-- Unfortunately, that doesn't mean we can just naively convert identifiers, for example:
--  - generated identifiers such as dictionary names in CoreFn can contain '$'
--  - module names can contain '.'
--  - keys might be quoted
--  - we shouldn't shadow Nix keywords, especially those that aren't also PureScript keywords
--  - we reserve any identifier starting with two leading underscores
--
-- Purescript identifiers:
-- https://github.com/purescript/purescript/blob/master/lib/purescript-cst/src/Language/PureScript/CST/Lexer.hs#L689
-- Nix identifiers:
-- https://github.com/cstrahan/tree-sitter-nix/blob/master/src/grammar.json#L19
module PureNix.Identifiers
  ( Var (..),
    mkVar,
    numberedVars,
    Key (..),
    identKey,
    stringKey,
    moduleKey,
    binderKey,
    numberedKeys,
  )
where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.PureScript as PS
import qualified Language.PureScript.PSString as PS

-- TODO rename to Binder, since this can occur in the LHS of a let-binding

-- | A valid (i.e. containing no illegal characters) variable binder.
-- Primarily constructed using 'mkVar'.
newtype Var = UnsafeVar {unVar :: Text}
  deriving newtype (IsString, Eq, Show)

identToText :: PS.Ident -> Text
identToText (PS.Ident t) = t
-- GenIdent is only used in PureScript for "unnamed" instances.
-- Originally, in PureScript, all instances needed to be named:
-- https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md#named-instances
-- This was relaxed in 0.14.2:
-- https://github.com/purescript/purescript/pull/4096
identToText (PS.GenIdent mvar n) = fromMaybe "__instance" mvar <> T.pack (show n)
identToText PS.UnusedIdent = error "impossible"

-- | Make a Nix variable binder from a CoreFn binder.
--
-- If a binder is a Nix keyword, we tick the binder with a hyphen.
-- Since PureScript does not allow binders to contain hyphens, this should be safe.
--
-- Additionally, CoreFn can put dollar signs in generated names.
-- We simply drop leading dollar signs, and the rest we convert to hyphens.
mkVar :: PS.Ident -> Var
mkVar = UnsafeVar . removeDollarSigns . tickKeywords . identToText
  where
    tickKeywords w
      | Set.member w keywords || T.isPrefixOf "__" w = w <> "-"
      | otherwise = w
    removeDollarSigns w =
      T.map (\c -> if c == '$' then '-' else c) $
        if T.isPrefixOf "$" w then T.tail w else w

keywords :: Set Text
keywords = Set.fromList (purenixIdents <> nixPrimops <> nixKeywords)
  where
    purenixIdents = ["module", "foreign"]
    -- These are not keywords in the sense that they can be shadowed
    -- For example, let true = false; in true is valid Nix.
    nixPrimops = ["builtins", "import", "false", "true"]
    -- keywords in nix:
    -- https://github.com/NixOS/nix/blob/90b2dd570cbd8313a8cf45b3cf66ddef2bb06e07/src/libexpr/lexer.l#L115-L124
    nixKeywords :: [Text]
    nixKeywords =
      ["if", "then", "else", "assert", "with", "let", "in", "rec", "inherit", "or"]

-- | A valid Nix attribute key
newtype Key = UnsafeKey {unKey :: Text}
  deriving newtype (IsString, Eq, Show)

moduleKey :: PS.ModuleName -> Key
moduleKey (PS.ModuleName mdl) = UnsafeKey $ "\"" <> mdl <> "\""

identKey :: PS.Ident -> Key
identKey = UnsafeKey . unVar . mkVar

stringKey :: PS.PSString -> Key
stringKey = UnsafeKey . PS.prettyPrintObjectKey

binderKey :: Var -> Key
binderKey = UnsafeKey . unVar

numberedText :: Text -> [Text]
numberedText prefix = fmap (\n -> prefix <> T.pack (show n)) [0 :: Int ..]

numberedVars :: Text -> [Var]
numberedVars = fmap UnsafeVar . numberedText

numberedKeys :: Text -> [Key]
numberedKeys = fmap UnsafeKey . numberedText
