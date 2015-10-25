module CPU.Detector.ReadBlocks (memReadBlock, regReadBlock) where
    
import CLaSH.Prelude hiding (Read)
import CPU.Defs (Addr, Reg, Read(..))
import CPU.Ops (Op(..), Fetched(..))
import CPU.Safety.Stages (Stage(D))

-- We should never get Ldr2 here, because it should have been re-written to an Ld
memReadBlock :: Fetched D -> Read Addr
memReadBlock d_op = case opOf d_op of
    Ld addr _ -> Read addr
    _         -> NoRead

regReadBlock :: Fetched D -> Read (Reg, Reg)
regReadBlock d_op = case opOf d_op of
    Add r1 r2 _ -> Read (r1, r2)
    Ldr1 r1 r2  -> Read (r1, r2)
    Jeq r1 r2 _ -> Read (r1, r2)
    St r1 _     -> Read (r1, r1)
    _           -> NoRead
