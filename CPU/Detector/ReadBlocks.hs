module CPU.Detector.ReadBlocks (memReadBlock, regReadBlock) where
    
import CLaSH.Prelude hiding (Read)
import CPU.Defs (Addr, Reg, Read(..))
import CPU.Ops (Op(..), Fetched(..))

-- We should never get Ldr2 here, because it should have been re-written to an Ld
memReadBlock :: Fetched -> Read Addr
memReadBlock fetched = case opOf fetched of
    Ld addr _ -> Read addr
    _         -> NoRead

regReadBlock :: Fetched -> Read (Reg, Reg)
regReadBlock fetched = case opOf fetched of
    Add r1 r2 _ -> Read (r1, r2)
    Ldr1 r1 r2  -> Read (r1, r2)
    Jeq r1 r2 _ -> Read (r1, r2)
    St r1 _     -> Read (r1, r1)
    _           -> NoRead
