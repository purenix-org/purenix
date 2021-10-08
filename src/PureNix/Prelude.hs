module PureNix.Prelude
  ( module X,
    LText,
  )
where

import Control.Applicative as X
import Control.Monad.Except as X
import Control.Monad.Reader as X
import Control.Monad.State as X
import Data.Bool as X (bool)
import Data.Map as X (Map)
import Data.Maybe as X (fromMaybe)
import Data.String as X (IsString (..))
import Data.Text as X (Text)
import qualified Data.Text.Lazy as LT
import Prelude as X

type LText = LT.Text
