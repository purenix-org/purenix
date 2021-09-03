module Nix.Convert
  ( Convert,
    ConversionError (..),
    convertModule,
  )
where

import Control.Monad.Except
import Language.PureScript.CoreFn as P
import Language.PureScript.Errors (SourceSpan)
import Nix.AST as N

data ConversionError = TODO
  deriving (Eq, Show)

type Convert = Either (SourceSpan, ConversionError)

throwAnn :: Ann -> ConversionError -> Convert a
throwAnn (spn, _, _, _) = throwAt spn

throwAt :: SourceSpan -> ConversionError -> Convert a
throwAt spn err = throwError (spn, err)

convertModule :: Module Ann -> Convert Nix
convertModule md = throwAt (moduleSourceSpan md) TODO

convertExpr :: Expr Ann -> Convert Nix
convertExpr (Literal _ lit) = undefined
