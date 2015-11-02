module CPU.Rewrite.RewriteBlocks (microfetchRewrite, dRewrite, rRewrite, xRewrite) where

import CLaSH.Prelude hiding (lookup)
import CPU.Defs (Reg, Addr(..), W(..), Jump(..), Predicted(..))
import CPU.Hazard.Hazard (hazard, opRegReads, opRegWrites, regConflict, opMemReads, opMemWrites, memConflict)
import CPU.Ops(Op(..), Fetched(..), invalidated)
import CPU.Cache.WriteCache (WriteCache, lookup)
import CPU.Safety.Stages(Stage(F,D,R,X), IsStage)

-- Only rewrites Ldrs. This lets us sometimes avoid having to use microcode
microfetchRewrite :: KnownNat n => WriteCache n Reg -> Fetched F -> Fetched F
microfetchRewrite cached f_op | (Ldr a b t) <- opOf f_op
                              , (Just av)   <- lookup cached a
                              , (Just bv)   <- lookup cached b
                                          = f_op {opOf = Ld (Addr $ w av + w bv) t}
                              | otherwise = f_op


ldr2Rewrite :: Fetched X -> Fetched D -> Fetched D
ldr2Rewrite x_op d_op = case opOf d_op of
    Ldr2 reg -> case opOf x_op of
        Ldr1Lit addr -> d_op {opOf = Ld addr reg}
        -- Something is horribly wrong if x_op isn't an Ldr1Lit
    _       -> d_op

generalRewrite :: (IsStage s, KnownNat m, KnownNat n) => WriteCache m Addr -> WriteCache n Reg -> Fetched s -> Fetched s
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

stallRewrite :: Fetched R -> Fetched X -> Fetched D -> (Fetched D, Bool)
stallRewrite r_op x_op d_op = if regHazard || memHazard 
    then (invalidated, True)
    else (d_op,        False)
    where 
    (d, r, x) = (opOf d_op, opOf r_op, opOf x_op)
    regHazard = hazard opRegReads opRegWrites regConflict d r x
    memHazard = hazard opMemReads opMemWrites memConflict d r x

jmpRewrite :: IsStage s => Jump -> Fetched s -> (Jump, Fetched s)
jmpRewrite (Jump pc) _      = (Jump pc, invalidated)
jmpRewrite NoJump    the_op = detectMisprediction the_op

-- Note: We actually need to keep Jmps now, because otherwise
-- It will think we severely mis-predicted a Nop.
detectMisprediction :: IsStage s => Fetched s -> (Jump, Fetched s)
detectMisprediction the_op = case opOf the_op of
    Jmp pc -> if predicted == pc
        then (NoJump, the_op) -- We got it right
        else (Jump pc, the_op {predictedOf = Predicted pc}) -- Got it wrong. Set the predicted to be correct
    Jeq _ _ _ -> (NoJump, the_op) -- Defer any opinion until it gets turned into a Jmp
    _ -> if predicted == plus1
        then (NoJump, the_op) -- All good
        else (Jump plus1, the_op {predictedOf = Predicted plus1})
    where
    predicted = prediction (predictedOf the_op)
    plus1 = pcOf the_op + 1

--dRewrite :: (KnownNat m, KnownNat n) => Fetched R -> Fetched X -> WriteCache m Addr -> WriteCache n Reg -> Jump -> Fetched D -> (Bool, Jump, Fetched D)
dRewrite :: Fetched R -> Fetched X -> WriteCache 8 Addr -> WriteCache 8 Reg -> Jump -> Fetched D -> (Bool, Jump, Fetched D)
dRewrite r_op x_op mem_cache cache jump op = (stall, jump', op'')
    where 
    (jump', op')  = jmpRewrite jump . generalRewrite mem_cache cache . ldr2Rewrite x_op $ op
    (op'', stall) = stallRewrite r_op x_op op'

rRewrite :: (KnownNat m, KnownNat n) => WriteCache m Addr -> WriteCache n Reg -> Jump -> Fetched R -> (Jump, Fetched R)
rRewrite mem_cache cache jump = jmpRewrite jump . generalRewrite mem_cache cache

-- We then also need to deal with the writeback result
-- Also dont' forget to put a cache detector on the output of all rewriters, as well as one after writeback
-- The only things that should come out of this are:
-- • Mov
-- • Halt
-- • Nop
-- • StLit
-- • Ldr1Lit (Writeback doesn't have to do anything; it's used by ldr2Rewrite)
-- -- • Jmp -- Nevermind, this gets handled by xRewrite's jmpRewrite
xSimplify :: W -> (W,W) -> Fetched X -> Fetched X
xSimplify mem (r1,r2) x_op = x_op {opOf = op'}
    where 
    op' = case opOf x_op of
        Add _ _ reg -> Mov (r1 + r2) reg
        Ld _ reg    -> Mov mem reg
        Jeq _ _ dpc -> if r1 == r2 then Jmp (pcOf x_op + dpc) else Nop
        St _ addr   -> StLit r1 addr
        Ldr1 _ _    -> Ldr1Lit (Addr . w $ (r1 + r2))
        otherOp     -> otherOp

xRewrite :: W -> (W,W) -> Jump -> Fetched X -> (Jump, Fetched X)
xRewrite mem (r1,r2) jump = jmpRewrite jump . xSimplify mem (r1,r2)
