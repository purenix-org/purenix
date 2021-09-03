module Lib where

import Data.Aeson (decode)
import Data.Aeson.Types (parseEither)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import System.Directory (createDirectoryIfMissing)
import Text.Pretty.Simple (pPrint)

defaultMain :: IO ()
defaultMain = do
  corefn <- readFile "../purescript-cabal-parser/output/Main/corefn.json"
  let maybeValue = decode $ encodeUtf8 $ pack corefn
  case maybeValue of
    Nothing -> error "wasn't able to decode corefn as json"
    Just value -> do
      let eitherModule = parseEither moduleFromJSON value
      case eitherModule of
        Left err -> error err
        Right (version, mdl) -> do
          putStrLn "successfully decoded purescript module"
          pPrint mdl

          -- TODO: Transform the purescript module into a .nix file
          createDirectoryIfMissing True "../generated"
          putStrLn "writing ../generated/Main.nix"
          writeFile "../generated/Main.nix" "{ myId = a: a; myNum = 3; }"

