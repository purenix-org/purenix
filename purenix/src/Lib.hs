{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Nix.Convert (convert)
import Nix.Prelude
import Nix.Print (renderExpr)
import qualified System.Directory as Dir
import qualified System.Exit as Sys
import System.FilePath ((</>))

defaultMain :: IO ()
defaultMain = do
  let moduleRoot = "../purescript-cabal-parser/output/"
  moduleDirs <- filter (/= "cache-db.json") <$> Dir.listDirectory moduleRoot
  forM_ moduleDirs $ \rel -> do
    let dir = moduleRoot </> rel
    let file = dir </> "corefn.json"
    putStrLn $ "Converting " <> file <> "..."
    value <- Aeson.eitherDecodeFileStrict file >>= either Sys.die pure
    (_version, module') <- either Sys.die pure $ parseEither moduleFromJSON value
    nix <- either (Sys.die . T.unpack) pure $ convert module'
    TL.writeFile (dir </> "default.nix") (renderExpr nix)
