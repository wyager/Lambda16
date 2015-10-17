module CPU.Detector.HaltBlock (haltBlock) where

import CLaSH.Prelude
import CPU.Ops(Op(..), Fetched(..))

haltBlock :: Fetched -> Bool
haltBlock fetched = opOf fetched == Halt