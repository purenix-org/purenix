module Lib where

import Data.Aeson (decode)
import Data.Aeson.Types (parseEither)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Nix.Convert (convertModule)
import Nix.Print (renderNix)
import System.Directory (createDirectoryIfMissing)
import qualified System.Environment as Env
import qualified System.Exit as Sys

defaultMain :: IO ()
defaultMain = do
  Env.getArgs >>= print
  corefn <- readFile "../purescript-cabal-parser/output/Main/corefn.json"
  -- TODO why is this lazy?
  let maybeValue = decode $ encodeUtf8 $ pack corefn
  case maybeValue of
    Nothing -> error "wasn't able to decode corefn as json"
    Just value -> do
      let eitherModule = parseEither moduleFromJSON value
      case eitherModule of
        Left err -> error err
        Right (_version, mdl) -> do
          putStrLn "successfully decoded purescript module"
          nix <- either (Sys.die . show) (pure . renderNix) $ convertModule mdl
          TL.putStrLn nix

          -- TODO: Transform the purescript module into a .nix file
          createDirectoryIfMissing True "../generated"
          putStrLn "writing ../generated/Main.nix"
          TL.writeFile "../generated/Main.nix" nix
