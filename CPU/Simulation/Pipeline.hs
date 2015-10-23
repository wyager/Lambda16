module CPU.Simulation.Pipeline (Pipeline(..)) where

import CPU.Ops(Op(..), Fetched(..))
import Text.Printf (printf)
import CLaSH.Prelude
import CPU.Safety.Stages (Stage(F,D,R,X))

data Pipeline = Pipeline (Fetched F) (Fetched D) (Fetched D) (Fetched R) (Fetched R) (Fetched X) -- uf, d, d', w, w', wb

instance Show Pipeline where
    show (Pipeline uf d d' w w' wb) = printf "VVVVVVVVVVVVVVVV\n%s [>] %s\n%s [>] %s\n%s [>] %s\n^^^^^^^^^^^^^^^^" (show uf) (show d) (show d') (show w) (show w') (show wb)