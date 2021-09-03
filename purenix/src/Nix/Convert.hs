{-# LANGUAGE OverloadedStrings #-}
-- TODO don't export
{-# LANGUAGE TupleSections #-}

module Nix.Convert
  ( convert,
    expr,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bool (bool)
import Data.Map
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.PureScript (Ident (..))
import qualified Language.PureScript as P
import Language.PureScript.CoreFn
import Language.PureScript.Errors (SourceSpan)
import Language.PureScript.PSString (PSString)
import qualified Nix.Expr as N

type Convert = ReaderT (FilePath, SourceSpan) (Either Text)

convert :: Module Ann -> Either Text N.Expr
convert (Module spn _comments _name path imports exports reexports _foreign decls) = runReaderT (module' imports exports reexports decls) (path, spn)

throw :: Text -> Convert a
throw err = ask >>= throwError . uncurry format
  where
    format fp spn =
      T.unlines
        [ mconcat ["Error in ", T.pack fp, " at ", P.displayStartEndPosShort spn, ":"],
          err
        ]

localSpan :: SourceSpan -> Convert a -> Convert a
localSpan spn = local (fmap $ const spn)

localAnn :: Ann -> Convert a -> Convert a
localAnn (spn, _, _, _) = localSpan spn

-- Module wrapper structure:
-- import:
-- let
--   bindingA = "foo"
--   bindingB = "bar"
-- in {
--   inherit bindingA;
--   inherit (import.moduleA) reExportA;
-- }

{-# ANN module' ("hlint: ignore" :: String) #-}
module' ::
  [(Ann, P.ModuleName)] ->
  [Ident] ->
  Map P.ModuleName [Ident] ->
  [Bind Ann] ->
  Convert N.Expr
module' _imports exports reexports decls =
  (liftA (N.abs "import"))
    ( (liftA2 N.let')
        (bindings decls)
        ( (liftA3 N.attrs)
            (traverse ident exports)
            (traverse (uncurry inheritFrom) (M.toList reexports))
            (pure mempty)
        )
    )
  where
    inheritFrom :: P.ModuleName -> [Ident] -> Convert (N.Expr, [N.Ident])
    inheritFrom (P.ModuleName m) exps = (N.sel (N.var "import") m,) <$> traverse ident exps

bindings :: [Bind Ann] -> Convert (Map N.Ident N.Expr)
bindings = fmap M.fromList . traverse binding . (>>= flatten)
  where
    binding :: (Ann, Ident, Expr Ann) -> Convert (N.Ident, N.Expr)
    binding (ann, i, e) = localAnn ann $ liftA2 (,) (ident i) (expr e)
    flatten :: Bind a -> [(a, Ident, Expr a)]
    flatten (NonRec a i e) = [(a, i, e)]
    flatten (Rec bs) = (\((a, i), e) -> (a, i, e)) <$> bs

expr :: Expr Ann -> Convert N.Expr
expr (Abs ann arg body) = localAnn ann $ liftA2 N.abs (ident arg) (expr body)
expr (Literal ann lit) = localAnn ann $ literal lit
expr (App ann f x) = localAnn ann $ liftA2 N.app (expr f) (expr x)
expr (Var ann (P.Qualified Nothing i)) = localAnn ann $ N.var <$> ident i
expr (Var ann (P.Qualified (Just (P.ModuleName m)) i)) = localAnn ann $ N.sel (N.sel (N.var "import") m) <$> ident i
expr (Accessor ann sel body) = localAnn ann $ flip N.sel (P.prettyPrintObjectKey sel) <$> expr body
expr (Let ann binds body) = localAnn ann $ liftA2 N.let' (bindings binds) (expr body)
expr (ObjectUpdate ann a b) = localAnn ann $ liftA2 N.update (expr a) (attrs b)
expr Case {} = throw "Cannot yet convert case expression"
expr Constructor {} = throw "Cannot yet convert constructors"

ident :: Ident -> Convert N.Ident
ident (Ident i) = pure i
ident (GenIdent mname n) = pure $ maybe id mappend mname (T.pack $ show n)
ident UnusedIdent = throw "Impossible: Encountered typechecking-only identifier"

attrs :: [(PSString, Expr Ann)] -> Convert N.Expr
attrs = fmap (N.attrs [] [] . M.fromList) . traverse attr
  where
    attr (string, body) = (P.prettyPrintString string,) <$> expr body

literal :: Literal (Expr Ann) -> Convert N.Expr
literal (NumericLiteral (Left n)) = pure $ N.num n
literal (NumericLiteral (Right _)) = throw "Encountered floating-point literal"
literal (StringLiteral str) = pure $ N.string $ P.prettyPrintString str
literal (CharLiteral chr) = pure $ N.string $ T.singleton chr
literal (BooleanLiteral b) = pure $ bool (N.var "false") (N.var "true") b
literal (ArrayLiteral arr) = N.list <$> traverse expr arr
literal (ObjectLiteral obj) = attrs obj
