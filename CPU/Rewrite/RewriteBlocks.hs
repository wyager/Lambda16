module CPU.Rewrite.RewriteBlocks (microfetchRewrite, decodeRewrite, waitRewrite, writebackRewrite) where

import CLaSH.Prelude hiding (lookup)
import CPU.Defs (Reg, Addr(..), W(..), Jump(..))
import CPU.Hazard.Hazard (hazard, opRegReads, opRegWrites, regConflict, opMemReads, opMemWrites, memConflict)
import CPU.Ops(Op(..))
import CPU.Cache.Cache (Cache, lookup)

-- Only rewrites Ldrs. This lets us sometimes avoid having to use microcode
microfetchRewrite :: KnownNat n => Op -> Cache n Reg -> Op
microfetchRewrite (Ldr a b t) cached = case (lookup cached a, lookup cached b) of
    (Just av, Just bv) -> Ld (Addr $ w av + w bv) t
    _                  -> Ldr a b t
microfetchRewrite op          _      = op

ldr2Rewrite :: Op -> Op -> Op
ldr2Rewrite writebackOp op = case op of
    Ldr2 reg -> case writebackOp of
        Ldr1Lit addr -> Ld addr reg
        -- Something is horribly wrong if writebackOp isn't an Ldr1Lit
    otherOp  -> otherOp

generalRewrite :: (KnownNat m, KnownNat n) => Cache m Addr -> Cache n Reg -> Op -> Op
generalRewrite mem_cached cached op = case op of
    Add r1 r2 r3     -> case (lookup cached r1, lookup cached r2) of
        (Just a, Just b) -> Mov (a+b) r3
        _                -> op
    Ldr1 r1 r2       -> case (lookup cached r1, lookup cached r2) of
        (Just a, Just b) -> Ldr1Lit $ Addr $ w (a + b)
        _                -> op
    Jeq r1 r2 pc' -> case (lookup cached r1, lookup cached r2) of
        (Just a, Just b) -> if a == b
                            then Jmp pc'
                            else Nop
        _                -> op
    St r1 addr pc -> case lookup cached r1 of
        Just w           -> StLit w addr pc
        _                -> op
    Ld addr reg  -> case lookup mem_cached addr of
        Just w           -> Mov w reg
        _                -> op
    otherOp              -> otherOp

stallRewrite :: Op -> Op -> Op -> (Op, Bool)
stallRewrite wait writeback decode = if regHazard || memHazard 
    then (Nop,    True)
    else (decode, False)
    where 
    regHazard = hazard opRegReads opRegWrites regConflict decode wait writeback
    memHazard = hazard opMemReads opMemWrites memConflict decode wait writeback

jmpRewrite :: Jump -> Op -> (Jump, Op)
jmpRewrite (Jump pc) _     = (Jump pc, Nop)
jmpRewrite NoJump (Jmp pc) = (Jump pc, Nop)
jmpRewrite NoJump op       = (NoJump,  op)

decodeRewrite :: (KnownNat m, KnownNat n) => Op -> Op -> Cache m Addr -> Cache n Reg -> Jump -> Op -> (Bool, Jump, Op)
decodeRewrite waitOp writebackOp mem_cache cache jump op = (stall, jump', op'')
    where 
    (jump', op')  = jmpRewrite jump . generalRewrite mem_cache cache . ldr2Rewrite writebackOp $ op
    (op'', stall) = stallRewrite waitOp writebackOp op'

waitRewrite :: (KnownNat m, KnownNat n) => Cache m Addr -> Cache n Reg -> Jump -> Op -> (Jump, Op)
waitRewrite mem_cache cache jump = jmpRewrite jump . generalRewrite mem_cache cache

-- We then also need to deal with the writeback result
-- Also dont' forget to put a cache detector on the output of all rewriters, as well as one after writeback
-- The only things that should come out of this are:
-- • Mov
-- • Halt
-- • Nop
-- • StLit
-- • Ldr1Lit (Writeback doesn't have to do anything; it's used by ldr2Rewrite)
-- -- • Jmp -- Nevermind, this gets handled by writebackRewrite's jmpRewrite
writebackSimplify :: W -> (W,W) -> Op -> Op
writebackSimplify mem (r1,r2) op = case op of
    Add _ _ reg  -> Mov (r1 + r2) reg
    Ld _ reg     -> Mov mem reg
    Jeq _ _ pc   -> if r1 == r2 then Jmp pc else Nop
    St _ addr pc -> StLit mem addr pc
    Ldr1 _ _     -> Ldr1Lit (Addr . w $ (r1 + r2))
    otherOp      -> otherOp

writebackRewrite :: W -> (W,W) -> Jump ->  Op -> (Jump, Op)
writebackRewrite mem (r1,r2) jump = jmpRewrite jump . writebackSimplify mem (r1,r2)
