module CPU.Simulation.Regs (regs) where

import qualified Prelude as P
import CLaSH.Prelude
import CLaSH.Sized.Vector

import CPU.Defs (S, Reg, W(..), Write(..))

regs :: Vec 16 W -> S (Reg, Reg) -> S (Write Reg) -> S (W, W)
regs initial r01 write = bundle (r0'', r1'')
    where
    (r0, r1) = unbundle r01
    write' = register NoWrite write
    regs = register initial (modify <$> regs <*> write')
    modify mem (Write addr word) = replace addr word mem
    modify mem NoWrite           = mem
    r0' = register 0 r0
    r0'' = register 0 (liftA2 (!!) regs r0')
    r1' = register 0 r1
    r1'' = register 0 (liftA2 (!!) regs r1')
