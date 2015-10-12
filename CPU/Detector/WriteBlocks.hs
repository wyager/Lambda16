module CPU.Detector.WriteBlocks (memWritebackBlock, regWritebackBlock) where
import CLaSH.Prelude
import CPU.Defs (Addr, Reg, Write(..))
import CPU.Ops (Op(..))

memWritebackBlock :: Op -> Write Addr
memWritebackBlock op = case op of
    StLit w addr pc -> Write addr w
    _ -> NoWrite

regWritebackBlock :: Op -> Write Reg
regWritebackBlock op = case op of
    Mov w reg -> Write reg w
    _ -> NoWrite
