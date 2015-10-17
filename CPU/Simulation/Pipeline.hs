module CPU.Simulation.Pipeline (Pipeline(..)) where

import CPU.Ops(Op(..), Fetched(..))
import Text.Printf (printf)
import CLaSH.Prelude

data Pipeline = Pipeline Fetched Fetched Fetched Fetched Fetched Fetched -- uf, d, d', w, w', wb

instance Show Pipeline where
    show (Pipeline uf d d' w w' wb) = printf "VVVVVVVVVVVVVVVV\n%s [>] %s\n%s [>] %s\n%s [>] %s\n^^^^^^^^^^^^^^^^" (show uf) (show d) (show d') (show w) (show w') (show wb)