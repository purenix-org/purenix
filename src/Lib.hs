{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Language.PureScript.CoreFn as P
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Nix.Convert (ModuleInfo (ModuleInfo), convert)
import Nix.Prelude
import Nix.Print (renderExpr)
import qualified System.Directory as Dir
import qualified System.Exit as Sys
import System.FilePath ((</>))
import qualified System.FilePath as FP

defaultMain :: IO ()
defaultMain = do
  -- let workdir = "../purescript-parser-combinator/"
  -- let workdir = "../purescript-unsafe-coerce/"
  -- let workdir = "../purescript-prelude/"
  let workdir = "."
  let moduleRoot = workdir </> "output"
  moduleDirs <- filter (/= "cache-db.json") <$> Dir.listDirectory moduleRoot
  forM_ moduleDirs $ \rel -> do
    let dir = moduleRoot </> rel
    let file = dir </> "corefn.json"
    putStrLn $ "Converting " <> file <> "..."
    value <- Aeson.eitherDecodeFileStrict file >>= either Sys.die pure
    (_version, module') <- either Sys.die pure $ parseEither moduleFromJSON value
    (nix, ModuleInfo usesFFI) <- either (Sys.die . T.unpack) pure $ convert module'
    TL.writeFile (dir </> "default.nix") (renderExpr nix)
    -- Copy FFI file, if one exists
    let modulePath = P.modulePath module'
        foreignSrc = workdir </> FP.replaceExtension modulePath "nix"
        foreignTrg = dir </> "foreign.nix"
    hasForeign <- Dir.doesFileExist foreignSrc
    case (hasForeign, usesFFI) of
      (True, True) -> Dir.copyFile foreignSrc foreignTrg
      (True, False) -> putStrLn $ "Warning: " <> modulePath <> " has an FFI file, but does not use FFI!"
      (False, True) -> putStrLn $ "Warning: " <> modulePath <> " calls foreign functions, but has no associated FFI file!"
      (False, False) -> pure ()
