module CPU.Rewrite.RewriteBlocks (microfetchRewrite, decodeRewrite, waitRewrite, writebackRewrite) where

import CLaSH.Prelude hiding (lookup)
import CPU.Defs (Reg, Addr(..), W(..), Jump(..), Predicted(..))
import CPU.Hazard.Hazard (hazard, opRegReads, opRegWrites, regConflict, opMemReads, opMemWrites, memConflict)
import CPU.Ops(Op(..), Fetched(..), invalidated)
import CPU.Cache.WriteCache (WriteCache, lookup)

-- Only rewrites Ldrs. This lets us sometimes avoid having to use microcode
microfetchRewrite :: KnownNat n => WriteCache n Reg -> Fetched -> Fetched
microfetchRewrite cached (Fetched (Ldr a b t) pc pred) = Fetched op' pc pred
    where 
    op' = case (lookup cached a, lookup cached b) of
        (Just av, Just bv) -> Ld (Addr $ w av + w bv) t
        _                  -> Ldr a b t
microfetchRewrite _      fetched                       = fetched

ldr2Rewrite :: Fetched -> Fetched -> Fetched
ldr2Rewrite wbFetched fetched = case opOf fetched of
    Ldr2 reg -> case opOf wbFetched of
        Ldr1Lit addr -> fetched {opOf = Ld addr reg}
        -- Something is horribly wrong if writebackOp isn't an Ldr1Lit
    _       -> fetched

generalRewrite :: (KnownNat m, KnownNat n) => WriteCache m Addr -> WriteCache n Reg -> Fetched -> Fetched
generalRewrite mem_cached cached (Fetched op pc pred) = Fetched op' pc pred
    where 
    op' = case op of
        Add r1 r2 r3     -> case (lookup cached r1, lookup cached r2) of
            (Just a, Just b) -> Mov (a+b) r3
            _                -> op
        Ldr1 r1 r2       -> case (lookup cached r1, lookup cached r2) of
            (Just a, Just b) -> Ldr1Lit $ Addr $ w (a + b)
            _                -> op
        Jeq r1 r2 dpc -> case (lookup cached r1, lookup cached r2) of
            (Just a, Just b) -> if a == b
                                then Jmp (pc + dpc)
                                else Nop
            _                -> op
        St r1 addr    -> case lookup cached r1 of
            Just w           -> StLit w addr
            _                -> op
        Ld addr reg  -> case lookup mem_cached addr of
            Just w           -> Mov w reg
            _                -> op
        otherOp              -> otherOp

stallRewrite :: Fetched -> Fetched -> Fetched -> (Fetched, Bool)
stallRewrite wait writeback decode = if regHazard || memHazard 
    then (invalidated, True)
    else (decode,      False)
    where 
    (d, w, wb) = (opOf decode, opOf wait, opOf writeback)
    regHazard = hazard opRegReads opRegWrites regConflict d w wb
    memHazard = hazard opMemReads opMemWrites memConflict d w wb

jmpRewrite :: Jump -> Fetched -> (Jump, Fetched)
jmpRewrite (Jump pc) _       = (Jump pc, invalidated)
jmpRewrite NoJump    fetched = detectMisprediction fetched

-- Note: We actually need to keep Jmps now, because otherwise
-- It will think we severely mis-predicted a Nop.
detectMisprediction :: Fetched -> (Jump, Fetched)
detectMisprediction fetched = case opOf fetched of
    Jmp pc -> if predicted == pc
        then (NoJump, fetched) -- We got it right
        else (Jump pc, fetched {predictedOf = Predicted pc}) -- Got it wrong. Set the predicted to be correct
    Jeq _ _ _ -> (NoJump, fetched) -- Defer any opinion until it gets turned into a Jmp
    _ -> if predicted == plus1
        then (NoJump, fetched) -- All good
        else (Jump plus1, fetched {predictedOf = Predicted plus1})
    where
    predicted = prediction (predictedOf fetched)
    plus1 = pcOf fetched + 1

decodeRewrite :: (KnownNat m, KnownNat n) => Fetched -> Fetched -> WriteCache m Addr -> WriteCache n Reg -> Jump -> Fetched -> (Bool, Jump, Fetched)
decodeRewrite waitOp writebackOp mem_cache cache jump op = (stall, jump', op'')
    where 
    (jump', op')  = jmpRewrite jump . generalRewrite mem_cache cache . ldr2Rewrite writebackOp $ op
    (op'', stall) = stallRewrite waitOp writebackOp op'

waitRewrite :: (KnownNat m, KnownNat n) => WriteCache m Addr -> WriteCache n Reg -> Jump -> Fetched -> (Jump, Fetched)
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
writebackSimplify :: W -> (W,W) -> Fetched -> Fetched
writebackSimplify mem (r1,r2) fetched = fetched {opOf = op'}
    where 
    op' = case opOf fetched of
        Add _ _ reg  -> Mov (r1 + r2) reg
        Ld _ reg     -> Mov mem reg
        Jeq _ _ dpc  -> if r1 == r2 then Jmp (pcOf fetched + dpc) else Nop
        St _ addr    -> StLit r1 addr
        Ldr1 _ _     -> Ldr1Lit (Addr . w $ (r1 + r2))
        otherOp      -> otherOp

writebackRewrite :: W -> (W,W) -> Jump ->  Fetched -> (Jump, Fetched)
writebackRewrite mem (r1,r2) jump = jmpRewrite jump . writebackSimplify mem (r1,r2)
