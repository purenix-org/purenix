{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module PureNix.Main.Run (getRunArg, run) where

import Control.Monad
import Data.Char (isLower, isUpper)
import qualified System.Environment as Env
import qualified System.Exit as Sys
import System.Process (proc, readCreateProcessWithExitCode)

data QualifiedFunction = QualifiedFunction
  { _moduleName :: String,
    _functionName :: String
  }

parseQualifiedFunction :: String -> Maybe QualifiedFunction
parseQualifiedFunction [] = Nothing
parseQualifiedFunction (c : cs)
  | isUpper c = go [c] cs
  | otherwise = Nothing
  where
    go prefix ('.' : t@(a : _))
      | isLower a = pure $ QualifiedFunction (reverse prefix) t
    go prefix (a : as) = go (a : prefix) as
    go _ [] = Nothing

getRunArg :: IO (Maybe QualifiedFunction)
getRunArg = do
  args <- Env.getArgs
  case args of
    ["--run", arg] -> case parseQualifiedFunction arg of
      Nothing -> Sys.die "Parse error, argument to --run does not appear to be a valid qualified function name"
      Just qf -> pure (Just qf)
    _ -> pure Nothing

run :: QualifiedFunction -> IO Sys.ExitCode
run (QualifiedFunction modName funcName) = do
  putStrLn "Running test..."
  (code, stdout, stderr) <- readCreateProcessWithExitCode (proc "nix-instantiate" ["--eval", "-E", show nixCommand]) ""
  unless (null stdout) $ putStrLn $ "stdout: " <> stdout
  unless (null stderr) $ putStrLn $ "stderr: " <> stderr
  pure code
  where
    nixCommand = "(import ./output/" <> modName <> ")." <> funcName
