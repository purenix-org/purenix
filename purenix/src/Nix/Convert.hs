module Nix.Convert
  ( Convert,
    ConversionError (..),
    convertModule,
    expr, -- TODO don't export
  )
where

import Control.Applicative
import Control.Monad.Except
import qualified Data.Text as T
import Language.PureScript (Ident (..))
import Language.PureScript.CoreFn as P
import Language.PureScript.Errors (SourceSpan)
import qualified Nix.Expr as N

data ConversionError = TODO
  deriving (Eq, Show)

type Convert = Either (SourceSpan, String)

throwAnn :: Ann -> String -> Convert a
throwAnn (spn, _, _, _) = throwAt spn

throwAt :: SourceSpan -> String -> Convert a
throwAt spn err = throwError (spn, err)

convertModule :: Module Ann -> Convert N.Expr
convertModule md = throwAt (moduleSourceSpan md) "まだ"

expr :: Expr Ann -> Convert N.Expr
expr (Abs ann arg body) = Nix <$> liftA2 N.Abs (ident ann arg) (expr body)
expr (Literal _ _) = literal lit

ident :: Ann -> Ident -> Convert N.Ident
ident _ (Ident i) = pure i
ident _ (GenIdent mname n) = pure $ maybe id mappend mname (T.pack $ show n)
ident ann UnusedIdent = throwAnn ann "Impossible: Encountered typechecking-only identifier"

literal :: Literal -> Convert N.Expr
