
module Nix.Prelude
  ( module X
  , LText
  ) where

import Prelude as X

import Control.Applicative as X
import Control.Monad.Except as X
import Control.Monad.Reader as X
import Control.Monad.State as X
import Data.Bool as X (bool)
import Data.Map as X (Map)
import Data.String as X (IsString (..))
import Data.Text as X (Text)
import qualified Data.Text.Lazy as LT

type LText = LT.Text
