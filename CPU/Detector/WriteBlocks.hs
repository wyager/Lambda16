module CPU.Detector.WriteBlocks (memWritebackBlock, regWritebackBlock) where
import CLaSH.Prelude
import CPU.Defs (Addr, Reg, Write(..))
import CPU.Ops (Op(..), Fetched(..))

memWritebackBlock :: Fetched -> Write Addr
memWritebackBlock fetched = case opOf fetched of
    StLit w addr -> Write addr w
    _            -> NoWrite

regWritebackBlock :: Fetched -> Write Reg
regWritebackBlock fetched = case opOf fetched of
    Mov w reg -> Write reg w
    _         -> NoWrite
