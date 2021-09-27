module Main (module Main, module Dep) where

import Dep

-- import Prelude

-- import Effect (Effect)
-- import Effect.Console (log)

-- main :: Effect Unit
-- main = do
--   log "🍝"

myId :: forall a. a -> a
myId a = a

myNum :: Int
myNum = myId myDep

foreign import mySubstring :: Int -> Int -> String -> String

-- | Output the string `"nix"`.
useMySubstring :: String
useMySubstring = mySubstring 0 3 "nixos"
