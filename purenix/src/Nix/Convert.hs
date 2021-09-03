{-# LANGUAGE OverloadedStrings #-}

module Nix.Convert
  ( convert,
    expr, -- TODO don't export
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.PureScript (Ident (..))
import qualified Language.PureScript as P
import Language.PureScript.CoreFn
import Language.PureScript.Errors (SourceSpan)
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

module' ::
  [(Ann, P.ModuleName)] ->
  [Ident] ->
  Map P.ModuleName [Ident] ->
  [Bind Ann] ->
  Convert N.Expr
module' imports exports reexports decls =
  pure $
    -- TODO avoid name clashes
    N.abs "imports" $
      N.let'
        (_ decls)
        $ N.attrs
          (_ exports)
          (_ reexports)
          mempty

expr :: Expr Ann -> Convert N.Expr
expr (Abs ann arg body) = localAnn ann $ liftA2 N.abs (ident arg) (expr body)
expr (Literal ann lit) = localAnn ann $ literal lit

ident :: Ident -> Convert N.Ident
ident (Ident i) = pure i
ident (GenIdent mname n) = pure $ maybe id mappend mname (T.pack $ show n)
ident UnusedIdent = throw "Impossible: Encountered typechecking-only identifier"

literal :: Literal (Expr Ann) -> Convert N.Expr
literal (NumericLiteral (Left n)) = pure $ N.num n
literal (NumericLiteral (Right _)) = throw "Encountered floating-point literal"
literal (StringLiteral str) = pure $ N.string $ P.prettyPrintString str
