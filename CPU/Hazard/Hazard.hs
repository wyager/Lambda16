module CPU.Hazard.Hazard (hazard, regConflict, opRegReads, opRegWrites, opMemReads, opMemWrites, memConflict) where

import CLaSH.Prelude
import CPU.Defs (Reg, Addr)
import CPU.Ops(Op(..))

regConflict :: (Reg, Reg) -> Reg -> Bool
regConflict (a,b) w = a == w || b == w

opRegReads :: Op -> Maybe (Reg, Reg)
opRegReads op = case op of
    Add  r1 r2 r3   -> Just (r1, r2)
    Ldr1 r1 r2      -> Just (r1, r2)
    Jeq  r1 r2 pc'  -> Just (r1, r2)
    St   r1 addr    -> Just (r1, r1)
    otherOp         -> Nothing

opRegWrites :: Op -> Maybe Reg
opRegWrites op = case op of
    Mov val reg -> Just reg
    Add _ _ reg -> Just reg
    Ld _    reg -> Just reg
    Ldr2    reg -> Just reg
    _           -> Nothing

memConflict :: Addr -> Addr -> Bool
memConflict = (==)

opMemReads :: Op -> Maybe Addr
opMemReads op = case op of
    Ld addr _ -> Just addr
    _         -> Nothing

opMemWrites :: Op -> Maybe Addr
opMemWrites op = case op of
    St _ addr      -> Just addr
    StLit _ addr   -> Just addr
    _              -> Nothing

hazard :: (Op -> Maybe r) -> (Op -> Maybe w) -> (r -> w -> Bool) -> Op -> Op -> Op -> Bool
hazard reads writes conflict decode wait writeback = conflicted wait || conflicted writeback
    where
    conflicted op = case conflict <$> reads decode <*> writes op of
        Nothing -> False -- decode doesn't read or op doesn't write
        Just conflicted -> conflicted -- Let "conflict" decide if there's a conflict
