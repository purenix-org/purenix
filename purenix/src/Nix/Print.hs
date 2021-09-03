module Nix.Print (renderNix) where

import Data.Text.Lazy (Text)
import Nix.AST (Nix)
import Text.Pretty.Simple (pShowNoColor)

renderNix :: Nix -> Text
renderNix = pShowNoColor
