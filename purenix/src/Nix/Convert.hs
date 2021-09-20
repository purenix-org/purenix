{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Nix.Convert (convert) where

import Data.Bitraversable
import qualified Data.Map as M
import qualified Data.Text as T
import Language.PureScript (Ident (..))
import qualified Language.PureScript as P
import Language.PureScript.CoreFn
import Language.PureScript.Errors (SourceSpan)
import Language.PureScript.PSString (PSString, decodeString)
import qualified Nix.Expr as N
import Nix.Prelude
import Nix.Util (nixKeywords)

type Convert = ReaderT (FilePath, P.ModuleName, SourceSpan) (Either Text)

convert :: Module Ann -> Either Text N.Expr
convert (Module spn _comments name path imports exports reexports foreign' decls) =
  runReaderT (module' name imports exports reexports foreign' decls) (path, name, spn)

throw :: Text -> Convert a
throw err = ask >>= throwError . format
  where
    format (fp, _, spn) =
      T.unlines
        [ T.concat ["Error in ", T.pack fp, " at ", P.displayStartEndPosShort spn, ":"],
          err
        ]

localSpan :: SourceSpan -> Convert a -> Convert a
localSpan spn = local (fmap $ const spn)

localAnn :: Ann -> Convert a -> Convert a
localAnn (spn, _, _, _) = localSpan spn

{-# ANN module' ("hlint: ignore Use list comprehension" :: String) #-}
module' ::
  P.ModuleName ->
  [(Ann, P.ModuleName)] ->
  [Ident] ->
  Map P.ModuleName [Ident] ->
  [Ident] ->
  [Bind Ann] ->
  Convert N.Expr
module' modName imports exports reexports foreign' decls = do
  let ffiFileBinding =
        if not (null foreign')
          then
            [ ( "foreign",
                N.app
                  (N.var "import")
                  (N.path ("./" <> P.runModuleName modName <> "foreign.nix"))
              )
            ]
          else []
  let importBinding =
        let toImport (_, P.ModuleName moduleName) = (moduleName, N.app (N.var "import") (N.path ("../" <> moduleName)))
         in [("module", N.attrs [] [] (toImport <$> imports))]
  ffiBinds <- traverse foreignBinding foreign'
  binds <- bindings decls
  expts <- traverse ident exports
  reexpts <- traverse (uncurry inheritFrom) (M.toList reexports)
  pure $
    N.let'
      (importBinding <> ffiFileBinding <> ffiBinds <> binds)
      (N.attrs expts reexpts mempty)
  where
    inheritFrom :: P.ModuleName -> [Ident] -> Convert (N.Expr, [N.Ident])
    inheritFrom (P.ModuleName m) exps = (N.sel (N.var "module") m,) <$> traverse ident exps

    foreignBinding :: Ident -> Convert (N.Ident, N.Expr)
    foreignBinding ffiIdent = do
      i <- ident ffiIdent
      pure (i, N.sel (N.var "foreign") i)

bindings :: [Bind Ann] -> Convert [(N.Ident, N.Expr)]
bindings = traverse binding . (>>= flatten)
  where
    binding :: (Ann, Ident, Expr Ann) -> Convert (N.Ident, N.Expr)
    binding (ann, i, e) = localAnn ann $ liftA2 (,) (ident i >>= checkKeyword) (expr e)
    flatten :: Bind a -> [(a, Ident, Expr a)]
    flatten (NonRec a i e) = [(a, i, e)]
    flatten (Rec bs) = (\((a, i), e) -> (a, i, e)) <$> bs

expr :: Expr Ann -> Convert N.Expr
expr (Abs ann arg body) = localAnn ann $ liftA2 N.lam (ident arg >>= checkKeyword) (expr body)
expr (Literal ann lit) = localAnn ann $ literal lit
expr (App ann f x) = localAnn ann $ liftA2 N.app (expr f) (expr x)
expr (Var ann (P.Qualified mqual name)) = localAnn ann $ do
  (_, thisModule, _) <- ask
  name' <- ident name
  pure $ case mqual of
    Just qual
      | qual /= thisModule -> N.sel (N.sel (N.var "module") (P.runModuleName qual)) name'
    _ -> N.var name'
expr (Accessor ann sel body) = localAnn ann $ flip N.sel (stringToKey sel) <$> expr body
expr (Let ann binds body) = localAnn ann $ liftA2 N.let' (bindings binds) (expr body)
expr (ObjectUpdate ann a b) = localAnn ann $ liftA2 (N.bin N.Update) (expr a) (attrs b)
expr (Constructor ann _ (P.ProperName dataName) fields) = localAnn ann $ N.constructor dataName <$> traverse ident fields
expr (Case ann exprs cases) =
  localAnn ann $ do
    exprs' <- traverse expr exprs
    cases' <- traverse (alternative exprs') cases
    (fp, _, spn) <- ask
    let patternCases = zip (N.numberedNames "__pattern") cases'
        patternFail =
          ( "__patternFail",
            N.app
              (N.builtin "throw")
              (N.string $ T.concat ["Pattern match failure in ", T.pack fp, " at ", P.displayStartEndPosShort spn])
          )
        patterns = patternCases <> [patternFail]
    pure $ N.let' patterns (foldr1 N.app (N.var . fst <$> patterns))

-- | Generates a matcher for a given case alternative, against the given list of scrutinees.
-- A matcher takes a failure continuation, and either calls the expression body with the matched names in scope, or if the matcher fails, the failure continutation.
alternative :: [N.Expr] -> CaseAlternative Ann -> Convert N.Expr
alternative scrutinees = go
  where
    go (CaseAlternative binders body) = do
      (patternChecks, patternBinds) <- zipBinders scrutinees binders
      body' <- unguard body (N.var "__fail")
      pure $
        N.lam "__fail" $
          case patternChecks of
            [] -> N.let' patternBinds body'
            _ ->
              N.cond
                (foldr1 (N.bin N.And) patternChecks)
                (N.let' patternBinds body')
                (N.var "__fail")

-- | Generates a matcher (see 'alternative') for a potentially guarded 'CaseAlternative' body.
-- For guards, we test every guard in order with the failure continuation as the final case.
unguard :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> N.Expr -> Convert N.Expr
unguard (Right body) _ = expr body
unguard (Left guardedBodies) failCase = do
  guardedBodies' <- traverse (bitraverse expr expr) guardedBodies
  pure $ foldr (uncurry N.cond) failCase guardedBodies'

zipBinders :: [N.Expr] -> [Binder Ann] -> Convert ([N.Expr], [(N.Ident, N.Expr)])
zipBinders exprs binds = mconcat <$> zipWithM unbinder binds exprs

-- | Turns a binder(/pattern) and a scrutinee into a pair of
--   - boolean expressions, that all return true iff the pattern applies
--   - the bindings produced by the pattern
unbinder :: Binder Ann -> N.Expr -> Convert ([N.Expr], [(N.Ident, N.Expr)])
unbinder (NullBinder _) _ = pure mempty
unbinder (VarBinder ann name) scrut = localAnn ann $ (\name' -> ([], [(name', scrut)])) <$> ident name
unbinder (ConstructorBinder ann _ (P.Qualified _ (P.ProperName tag)) fields) scrut =
  localAnn ann $
    mappend ([N.bin N.Equals (N.sel scrut "__tag") (N.string tag)], []) . mconcat <$> zipWithM (\binder field -> unbinder binder (N.sel scrut field)) fields N.constructorFieldNames
unbinder (NamedBinder ann name binder) scrut = localAnn ann $ do
  name' <- ident name
  mappend ([], [(name', scrut)]) <$> unbinder binder scrut
unbinder (LiteralBinder ann lit) scrut' = localAnn ann $ litBinder lit scrut'
  where
    litBinder :: Literal (Binder Ann) -> N.Expr -> Convert ([N.Expr], [(N.Ident, N.Expr)])
    litBinder (NumericLiteral (Left n)) scrut = pure ([N.bin N.Equals scrut (N.int n)], [])
    litBinder (NumericLiteral (Right x)) scrut = pure ([N.bin N.Equals scrut (N.double x)], [])
    litBinder (StringLiteral str) scrut = (\str' -> ([N.bin N.Equals scrut (N.string str')], [])) <$> string str
    litBinder (CharLiteral char) scrut = pure ([N.bin N.Equals scrut (N.string (T.singleton char))], [])
    litBinder (BooleanLiteral True) scrut = pure ([scrut], [])
    litBinder (BooleanLiteral False) scrut = pure ([N.negate scrut], [])
    litBinder (ArrayLiteral as) scrut =
      mappend ([N.bin N.Equals (N.app (N.builtin "length") scrut) (N.int (fromIntegral n))], []) . mconcat
        <$> zipWithM (\binder ix -> unbinder binder (elemAt scrut ix)) as [0 :: Integer ..]
      where
        n = length as
        elemAt list ix = N.app (N.app (N.builtin "elemAt") list) (N.int ix)
    litBinder (ObjectLiteral fields) scrut = mconcat <$> traverse (\(field, binder) -> unbinder binder (N.sel scrut (stringToKey field))) fields

ident :: Ident -> Convert N.Ident
ident (Ident i) = pure i
-- GenIdent is only used in PureScript for "unnamed" instances.
-- Originally, in PureScript, all instances needed to be named:
-- https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md#named-instances
-- This was relaxed in 0.14.2:
-- https://github.com/purescript/purescript/pull/4096
-- TODO: We'll have to make sure that no identifier are created that are _only_
-- an integer (when mname is Nothing), since they can't be used in Nix.
ident (GenIdent mname n) = pure $ maybe id mappend mname (T.pack $ show n)
ident UnusedIdent = throw "Impossible: Encountered typechecking-only identifier"

checkKeyword :: N.Ident -> Convert N.Ident
checkKeyword w
  | w `elem` purenixIdents = throw $ "binder " <> w <> " is a special identifier in purenix"
  | w `elem` nixKeywords = throw $ "binder " <> w <> " is a nix keyword"
  | w `elem` nixPrimops = throw $ "binder " <> w <> " is a nix primop.  You probably don't want to shadow this."
  | otherwise = pure w
  where
    -- These are keywords in either purenix or nix itself, and therefore shouldn't be shadowed.
    -- We could silently rename, but that's fragile, so for now we just warn not to use these.
    -- Note that with the exception of "builtins", everything without leading underscores are all keywords in purescript as well, so this shouldn't generally be an issue.
    -- TODO __fieldX keywords
    -- TODO Check how this is handled for attrset keys
    purenixIdents = ["module", "__tag", "foreign"]
    nixPrimops = ["builtins", "import", "false", "true"]

attrs :: [(PSString, Expr Ann)] -> Convert N.Expr
attrs = fmap (N.attrs [] []) . traverse attr
  where
    attr (string, body) = (stringToKey string,) <$> expr body

stringToKey :: PSString -> N.Ident
stringToKey = removeQuotes . P.prettyPrintObjectKey
  where
    removeQuotes :: Text -> Text
    removeQuotes t = fromMaybe t $ T.stripPrefix "\"" =<< T.stripSuffix "\"" t

string :: PSString -> Convert Text
string str = case decodeString str of
  Nothing -> throw "String contained lone surrogates"
  Just x -> pure x

literal :: Literal (Expr Ann) -> Convert N.Expr
literal (NumericLiteral (Left n)) = pure $ N.int n
literal (NumericLiteral (Right n)) = pure $ N.double n
literal (StringLiteral str) = N.string <$> string str
literal (CharLiteral chr) = pure $ N.string $ T.singleton chr
literal (BooleanLiteral b) = pure $ bool (N.var "false") (N.var "true") b
literal (ArrayLiteral arr) = N.list <$> traverse expr arr
literal (ObjectLiteral obj) = attrs obj
