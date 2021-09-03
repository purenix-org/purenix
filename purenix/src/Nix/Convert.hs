{-# LANGUAGE OverloadedStrings #-}

module Nix.Convert
  ( convert,
    expr, -- TODO don't export
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Language.PureScript (Ident (..))
import qualified Language.PureScript as P
import Language.PureScript.CoreFn
import Language.PureScript.Errors (SourceSpan)
import qualified Nix.Expr as N

type Convert = ReaderT SourceSpan (Either (SourceSpan, String))

convert :: Module Ann -> Either Text N.Expr
convert m = case runReaderT (module' m) (moduleSourceSpan m) of
  Left (spn, err) -> Left $ T.unlines ["Error at " <> P.displayStartEndPosShort spn <> ":", T.pack err]
  Right e -> pure e

throw :: String -> Convert a
throw err = ask >>= \spn -> throwError (spn, err)

localSpan :: SourceSpan -> Convert a -> Convert a
localSpan spn = local (const spn)

localAnn :: Ann -> Convert a -> Convert a
localAnn (spn, _, _, _) = local (const spn)

module' :: Module Ann -> Convert N.Expr
module' md = localSpan (moduleSourceSpan md) $ throw "Not yet implemented"

expr :: Expr Ann -> Convert N.Expr
expr (Abs ann arg body) = local' ann $ liftA2 N.Abs (ident arg) (expr body)
expr (Literal ann lit) = localAnn ann $ literal lit

local' :: Ann -> Convert (N.ExprF N.Expr) -> Convert N.Expr
local' = undefined

ident :: Ident -> Convert N.Ident
ident (Ident i) = pure i
ident (GenIdent mname n) = pure $ maybe id mappend mname (T.pack $ show n)
ident UnusedIdent = throw "Impossible: Encountered typechecking-only identifier"

literal :: Literal (Expr Ann) -> Convert N.Expr
literal (NumericLiteral (Left n)) = pure $ N.Expr $ N.Num n
literal (NumericLiteral (Right _)) = throw "Encountered floating-point literal"
literal (StringLiteral str) = pure $ N.Expr $ N.String $ P.prettyPrintString str
