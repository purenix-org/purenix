module Main where

-- import Prelude

-- import Effect (Effect)
-- import Effect.Console (log)

-- main :: Effect Unit
-- main = do
--   log "🍝"

myId :: forall a. a -> a
myId a = a

myNum :: Int
myNum = 3
