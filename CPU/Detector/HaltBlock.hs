module CPU.Detector.HaltBlock (haltBlock) where

import CLaSH.Prelude
import CPU.Ops(Op(..))

haltBlock :: Op -> Bool
haltBlock Halt = True
haltBlock _    = False