{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the conversions from purescript identifiers to (pure)nix identifiers.
-- Nix's rules for identifiers are strictly more lenient than purescripts (see specifications below), since nix allows - in non-head characters.
-- This makes it so that in general, we can just convert identifiers.
-- However, there are a number of edge cases that need to be handled:
--  - generated dictionary names will contain '$'
--  - module names contain '.'
--  - we should not shadow "builtins", or identifiers starting with "__". There are of course more identifiers, that could techincally be shadowed, but they tend to overlap with purescript's.
--
-- In general, once a purescript construct has been converted into a Key/Var, no more work is required.
-- That is, after turning them into Text, they can be used directly by the printer, no more work needs to be done to escape them.
--
-- Purescript identifiers:
-- https://github.com/purescript/purescript/blob/master/lib/purescript-cst/src/Language/PureScript/CST/Lexer.hs#L689
-- Nix identifiers:
-- https://github.com/cstrahan/tree-sitter-nix/blob/master/src/grammar.json#L19
module Nix.Identifiers
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

mkVar :: PS.Ident -> Var
mkVar = UnsafeVar . removeDollarSigns . tickKeywords . identToText
  where
    tickKeywords w
      | Set.member w keywords || T.isPrefixOf "__" w = w <> "-"
      | otherwise = w
    removeDollarSigns w =
      T.map (\c -> if c == '$' then '-' else c) $
        if T.isPrefixOf "$" w then "S" <> w else w

keywords :: Set Text
keywords = Set.fromList (purenixIdents <> nixPrimops <> nixKeywords)
  where
    -- These are keywords in either purenix or nix itself, and therefore shouldn't be shadowed.
    -- We could silently rename, but that's fragile, so for now we just warn not to use these.
    -- Note that with the exception of "builtins", everything without leading underscores are all keywords in purescript as well, so this shouldn't generally be an issue.
    purenixIdents = ["module", "foreign"]
    nixPrimops = ["builtins", "import", "false", "true"]
    -- keywords in nix:
    -- https://github.com/NixOS/nix/blob/90b2dd570cbd8313a8cf45b3cf66ddef2bb06e07/src/libexpr/lexer.l#L115-L124
    nixKeywords :: [Text]
    nixKeywords =
      ["if", "then", "else", "assert", "with", "let", "in", "rec", "inherit", "or"]

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
