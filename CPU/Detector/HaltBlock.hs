module CPU.Detector.HaltBlock (haltBlock) where

import CLaSH.Prelude
import CPU.Ops(Op(..), Fetched(..))
import CPU.Safety.Stages (Stage(X))

haltBlock :: Fetched X -> Bool
haltBlock x_op = opOf x_op == Halt