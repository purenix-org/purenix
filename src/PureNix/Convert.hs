{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureNix.Convert (convert, ModuleInfo (..)) where

import Data.Bitraversable
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Internal.Search (indices)
import Language.PureScript (Ident (..))
import qualified Language.PureScript as P
import Language.PureScript.CoreFn
import Language.PureScript.Errors (SourceSpan)
import Language.PureScript.PSString (PSString (toUTF16CodeUnits))
import qualified PureNix.Expr as N
import qualified PureNix.Identifiers as N
import PureNix.Prelude

-- The StateT here serves the role of a CPS'd WriterT
type Convert =
  ReaderT
    (FilePath, P.ModuleName, SourceSpan)
    (State ModuleInfo)

data ModuleInfo = ModuleInfo
  { usesFFI :: Bool,
    interpolatedStrings :: Set SourceSpan
  }
  deriving (Eq, Show)

instance Semigroup ModuleInfo where ModuleInfo fa ia <> ModuleInfo fb ib = ModuleInfo (fa || fb) (ia <> ib)

instance Monoid ModuleInfo where mempty = ModuleInfo False mempty

tell :: ModuleInfo -> Convert ()
tell m = modify (mappend m)

convert :: Module Ann -> (N.Expr, ModuleInfo)
convert (Module spn _comments name path imports exports reexports foreign' decls) =
  flip runState mempty $
    flip runReaderT (path, name, spn) $
      module' name imports exports reexports foreign' decls

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
module' thisModule imports exports reexports foreign' decls = do
  let importBinding =
        let attrs =
              [ (N.moduleKey mdl, N.app (N.var "import") (N.path ("../" <> P.runModuleName mdl)))
                | (_, mdl) <- imports,
                  mdl /= thisModule,
                  mdl /= P.ModuleName "Prim"
              ]
         in ("module", N.attrs [] [] attrs)
      ffiBinds = foreignBinding <$> foreign'
      expts = N.mkVar <$> exports
      reexpts = uncurry inheritFrom <$> M.toList reexports
  ffiFileBinding <-
    if null foreign'
      then pure []
      else [("foreign", N.app (N.var "import") (N.path "./foreign.nix"))] <$ tell mempty {usesFFI = True}
  binds <- bindings decls
  pure $
    N.let'
      (importBinding : ffiFileBinding <> ffiBinds <> binds)
      (N.attrs expts reexpts mempty)
  where
    inheritFrom :: P.ModuleName -> [Ident] -> (N.Expr, [N.Key])
    inheritFrom m exps = (N.sel (N.var "module") (N.moduleKey m), N.identKey <$> exps)

    foreignBinding :: Ident -> (N.Var, N.Expr)
    foreignBinding ffiIdent = (N.mkVar ffiIdent, N.sel (N.var "foreign") (N.identKey ffiIdent))

bindings :: [Bind Ann] -> Convert [(N.Var, N.Expr)]
bindings = traverse binding . (>>= flatten)
  where
    binding :: (Ann, Ident, Expr Ann) -> Convert (N.Var, N.Expr)
    binding (ann, i, e) = localAnn ann $ fmap (N.mkVar i,) (expr e)
    flatten :: Bind a -> [(a, Ident, Expr a)]
    flatten (NonRec a i e) = [(a, i, e)]
    flatten (Rec bs) = (\((a, i), e) -> (a, i, e)) <$> bs

expr :: Expr Ann -> Convert N.Expr
expr (Abs ann arg body) = localAnn ann $ fmap (N.lam (N.mkVar arg)) (expr body)
expr (Literal ann lit) = localAnn ann $ literal lit
-- Newtype wrappers can always be removed.
expr (App ann (Var (_, _, _, Just IsNewtype) _) x) = localAnn ann (expr x)
expr (App ann f x) = localAnn ann $ liftA2 N.app (expr f) (expr x)
expr (Var ann (P.Qualified mqual name)) = localAnn ann $ do
  (_, thisModule, _) <- ask
  pure $ case mqual of
    Just qual
      | qual /= thisModule -> N.sel (N.sel (N.var "module") (N.moduleKey qual)) (N.identKey name)
    _ -> N.var (N.mkVar name)
expr (Accessor ann sel body) = localAnn ann $ flip N.sel (N.stringKey sel) <$> expr body
expr (Let ann binds body) = localAnn ann $ liftA2 N.let' (bindings binds) (expr body)
expr (ObjectUpdate ann a b) = localAnn ann $ liftA2 (N.bin N.Update) (expr a) (attrs b)
expr (Constructor _ _ (P.ProperName dataName) fields) = pure $ N.constructor dataName (N.mkVar <$> fields)
expr (Case ann exprs cases) =
  localAnn ann $ do
    exprs' <- traverse expr exprs
    cases' <- traverse (alternative exprs') cases
    (fp, _, spn) <- ask
    let patternCases = zip (N.numberedVars "__pattern") cases'
        patternFail =
          ( "__patternFail",
            N.app
              (N.builtin "throw")
              (N.string $ T.concat ["Pattern match failure in ", T.pack fp, " at ", P.displayStartEndPosShort spn])
          )
        patterns = patternCases <> [patternFail]
    pure $
      N.let'
        patterns
        (foldr1 N.app (N.var . fst <$> patterns))

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

zipBinders :: [N.Expr] -> [Binder Ann] -> Convert ([N.Expr], [(N.Var, N.Expr)])
zipBinders exprs binds = mconcat <$> zipWithM unbinder binds exprs

-- | Turns a binder(/pattern) and a scrutinee into a pair of
--   - boolean expressions, that all return true iff the pattern applies
--   - the bindings produced by the pattern
unbinder :: Binder Ann -> N.Expr -> Convert ([N.Expr], [(N.Var, N.Expr)])
unbinder (NullBinder _) _ = pure mempty
unbinder (VarBinder _ name) scrut = pure $ (\name' -> ([], [(name', scrut)])) $ N.mkVar name
unbinder (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [field]) scrut = unbinder field scrut
unbinder (ConstructorBinder ann _ (P.Qualified _ (P.ProperName tag)) fields) scrut =
  localAnn ann $
    mappend ([N.bin N.Equals (N.sel scrut "__tag") (N.string tag)], []) . mconcat <$> zipWithM (\binder field -> unbinder binder (N.sel scrut field)) fields (N.numberedKeys "__field")
unbinder (NamedBinder ann name binder) scrut = localAnn ann $ do
  mappend ([], [(N.mkVar name, scrut)]) <$> unbinder binder scrut
unbinder (LiteralBinder ann lit) scrut' = localAnn ann $ litBinder lit scrut'
  where
    litBinder :: Literal (Binder Ann) -> N.Expr -> Convert ([N.Expr], [(N.Var, N.Expr)])
    litBinder (NumericLiteral (Left n)) scrut = pure ([N.bin N.Equals scrut (N.int n)], [])
    litBinder (NumericLiteral (Right x)) scrut = pure ([N.bin N.Equals scrut (N.double x)], [])
    litBinder (StringLiteral str) scrut = (\str' -> ([N.bin N.Equals scrut (N.string str')], [])) <$> string str
    litBinder (CharLiteral char) scrut = pure ([N.bin N.Equals scrut (N.string (T.singleton char))], [])
    litBinder (BooleanLiteral True) scrut = pure ([scrut], [])
    litBinder (BooleanLiteral False) scrut = pure ([N.not' scrut], [])
    litBinder (ArrayLiteral as) scrut =
      mappend ([N.bin N.Equals (N.app (N.builtin "length") scrut) (N.int (fromIntegral n))], []) . mconcat
        <$> zipWithM (\binder ix -> unbinder binder (elemAt scrut ix)) as [0 :: Integer ..]
      where
        n = length as
        elemAt list ix = N.app (N.app (N.builtin "elemAt") list) (N.int ix)
    litBinder (ObjectLiteral fields) scrut = mconcat <$> traverse (\(field, binder) -> unbinder binder (N.sel scrut (N.stringKey field))) fields

attrs :: [(PSString, Expr Ann)] -> Convert N.Expr
attrs = fmap (N.attrs [] []) . traverse attr
  where
    attr (string, body) = (N.stringKey string,) <$> expr body

string :: PSString -> Convert Text
string str = do
  let decoded = T.pack . map (toEnum . fromIntegral) . toUTF16CodeUnits $ str
  when (mightContainInterpolation decoded) $ do
    (_, _, spn) <- ask
    tell mempty {interpolatedStrings = S.singleton spn}
  pure decoded
  where
    mightContainInterpolation :: Text -> Bool
    mightContainInterpolation t = case indices "${" t of
      [] -> False
      (ixOpen : _) -> any (> ixOpen) $ indices "}" t

literal :: Literal (Expr Ann) -> Convert N.Expr
literal (NumericLiteral (Left n)) = pure $ N.int n
literal (NumericLiteral (Right n)) = pure $ N.double n
literal (StringLiteral str) = N.string <$> string str
literal (CharLiteral chr) = pure $ N.string $ T.singleton chr
literal (BooleanLiteral b) = pure $ bool (N.var "false") (N.var "true") b
literal (ArrayLiteral arr) = N.list <$> traverse expr arr
literal (ObjectLiteral obj) = attrs obj
