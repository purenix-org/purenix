{-# LANGUAGE NoImplicitPrelude #-}

module Nix.Util (stripAnnMod) where

import Nix.Prelude

import Language.PureScript.CoreFn (Bind, Module(Module))
import Language.PureScript.Names (ModuleName)

-- | Strip annotations from a 'Module'.  Helpful for pretty-printing modules for
-- debugging.
stripAnnMod :: Module a -> Module ()
stripAnnMod (Module spn comments name path imports exports reexports foreign' decls) =
  Module spn comments name path (fmap stripAnnImport imports) exports reexports foreign' (fmap stripAnnBind decls)

-- | Strip annotations from a let or module binding.
stripAnnBind :: Bind a -> Bind ()
stripAnnBind = fmap (const ())

-- | Strip an annotation from an import.
stripAnnImport :: (a, ModuleName) -> ((), ModuleName)
stripAnnImport (_, modName) = ((), modName)
