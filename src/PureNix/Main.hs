module PureNix.Main (defaultMain) where

import qualified PureNix.Main.Build as Build
import qualified PureNix.Main.Run as Run

defaultMain :: IO ()
defaultMain = do
  Build.build
  Run.getRunArg >>= mapM_ Run.run
