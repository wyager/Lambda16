module CPU.Simulation.Pipeline (Pipeline(..)) where

import CPU.Ops(Op(..))
import Text.Printf (printf)
import CLaSH.Prelude

data Pipeline = Pipeline Op Op Op Op Op Op -- uf, d, d', w, w', wb

instance Show Pipeline where
    show (Pipeline uf d d' w w' wb) = printf "#   %s [>] %s   [^]   %s [>] %s   [^]   %s [>] %s" (show uf) (show d) (show d') (show w) (show w') (show wb)