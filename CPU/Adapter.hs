{-# LANGUAGE LambdaCase #-}
module Adapter (topEntity) where

import CPU.CPU(cpu)
import CLaSH.Prelude
import CPU.Defs(S, Addr(..), W(..), Reg(..), Read(..), Write(..), PC(..))

type Bv = BitVector 16 
type Rv = BitVector 4

a $>> f = f <$> a

-- Provides a constant interface for plugging into the given cpu.v
topEntity :: S (Bv, Bv) -> S (Bv, Bv) -> S(Bv, Bv, Rv, Rv, (Bool, Bv, Bv), (Bool, Rv, Bv), Bool)
topEntity mem regs = bundle (mr1, mr2, rr1, rr2, memwrite, regwrite, halt)
    where
    dup f (a,b) = (f a, f b)
    (rpc, raddr, rregs, wmem, wregs, halt, _) = unbundle $ cpu (dup W <$> mem) (dup W <$> regs)
    mr1 = rpc $>> \(PC pc) -> pc
    mr2 = raddr $>> \case 
        Read (Addr a) -> a
        NoRead -> 0
    (rr1, rr2) = unbundle $ rregs $>> \case
        Read (Reg r1, Reg r2) -> (r1, r2)
        NoRead                -> (0,0)
    memwrite = wmem $>> \case
        Write (Addr a) (W w) -> (True, a, w)
        NoWrite              -> (False, 0, 0)
    regwrite = wregs $>> \case
        Write (Reg r) (W w) -> (True, r, w)
        NoWrite             -> (False, 0, 0)
