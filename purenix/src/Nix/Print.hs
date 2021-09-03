module Nix.Print (renderNix) where

import Data.Text.Lazy (Text)
import Nix.Expr (Expr)
import Text.Pretty.Simple (pShowNoColor)

renderNix :: Expr -> Text
renderNix = pShowNoColor
