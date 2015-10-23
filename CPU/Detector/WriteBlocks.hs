module CPU.Detector.WriteBlocks (memWritebackBlock, regWritebackBlock) where
import CLaSH.Prelude
import CPU.Defs (Addr, Reg, Write(..))
import CPU.Ops (Op(..), Fetched(..))
import CPU.Safety.Stages (Stage(X))

memWritebackBlock :: Fetched X -> Write Addr
memWritebackBlock fetched = case opOf fetched of
    StLit w addr -> Write addr w
    _            -> NoWrite

regWritebackBlock :: Fetched X -> Write Reg
regWritebackBlock fetched = case opOf fetched of
    Mov w reg -> Write reg w
    _         -> NoWrite
