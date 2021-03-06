module CPU.Fetch.MicroFetch (microfetch, test) where

import CLaSH.Prelude hiding (empty)
import CPU.Defs (S, W, PC, Jump(..), Write(..), Reg, Predicted(..))
import CPU.Rewrite.Microcode (microcode)
import CPU.Fetch.Fetch (fetch)
import CPU.Ops (Op(..), package, Fetched(..), invalidated)
-- Testing
import CPU.Simulation.RAM (ram)
import CPU.Cache.WriteCache as WC (WriteCache, empty)
import CPU.Rewrite.RewriteBlocks (microfetchRewrite)
import CPU.Fetch.SimplePredictor as PR (PCHash, Predictor, empty)
import qualified Prelude as P
import CPU.Safety.Stages (Stage(F,X))

microfetch :: (KnownNat n) => S (Fetched X) -> S (WriteCache n Reg) -> S W -> S Bool -> S Jump -> S (PC, (Fetched F))
microfetch x_op cache mem stall jmp = bundle (mem_read_addr, f_op)
    where
    (mem_read_addr, read_data) = unbundle $ fetch x_op mem micro_stall jmp
    -- Removes Ldrs for which we already know r[a] and r[b] so they don't get microcoded
    rewritten = microfetchRewrite <$> cache <*> (package <$> read_data) 
    (micro_stall, f_op) = unbundle $ microcode stall rewritten jumping
    jumping = (\x -> if x == NoJump then False else True) <$> jmp

empty' :: Signal (WriteCache 0 Reg)
empty' = signal WC.empty

empty'' :: Signal (Fetched X) -- (Predictor 0 0)
empty'' = signal invalidated --PR.empty

test :: Either String ()
test = test1 >> test2 >> test3 >> test4 >> test5 >> test6 >> test7 >> test8

test1 :: Either String ()
test1 = if actual == expected
    then Right ()
    else Left $ P.concat ["Microfetch test 1 failure.\n", "expected: ", show expected, "\nactual: ", show actual]
    where
    ram_contents = repeat 0
    mem = ram ram_contents
    (instrs,_) = unbundle $ mem pc 0 (signal NoWrite)
    (pc, ops)  = unbundle $ microfetch empty'' empty' instrs (signal False) (signal NoJump)
    actual = P.map opOf $ sampleN 4 ops
    expected = [Nop, Nop, Mov 0 0, Mov 0 0]

test2 :: Either String ()
test2 = if actual == expected
    then Right ()
    else Left $ P.concat ["Microfetch test 2 failure.\n", "expected: ", show expected, "\nactual: ", show actual]
    where
    ram_contents = (0x1234 :> 0x2345 :> Nil) ++ repeat 0
    mem = ram ram_contents
    (instrs,_) = unbundle $ mem pc 0 (signal NoWrite)
    (pc, ops)  = unbundle $ microfetch empty'' empty' instrs (signal False) (signal NoJump)
    actual = P.map opOf $ sampleN 4 ops
    expected = [Nop, Nop, Add 2 3 4, Jmp 0x345]

test3 :: Either String ()
test3 = if actual == expected
    then Right ()
    else Left $ P.concat ["Microfetch test 3 failure.\n", "expected: ", show expected, "\nactual: ", show actual]
    where
    ram_contents = (0x1234 :> 0x2345 :> Nil) ++ repeat 0
    mem = ram ram_contents
    (instrs,_) = unbundle $ mem pc 0 (signal NoWrite)
    (pc, ops)  = unbundle $ microfetch empty'' empty' instrs stall (signal NoJump)
    stall = fromList [False, False, True, True, False, False]
    actual = P.map opOf $ sampleN 6 ops
    expected = [Nop, Nop, Add 2 3 4, Add 2 3 4, Add 2 3 4, Jmp 0x345]

test4 :: Either String ()
test4 = if actual == expected
    then Right ()
    else Left $ P.concat ["Microfetch test 4 failure.\n", "expected: ", show expected, "\nactual: ", show actual]
    where
    ram_contents = (0x1234 :> 0x2345 :> Nil) ++ repeat 0
    mem = ram ram_contents
    (instrs,_) = unbundle $ mem pc 0 (signal NoWrite)
    (pc, ops)  = unbundle $ microfetch empty'' empty' instrs stall (signal NoJump)
    stall = fromList [False, False, True, False, False]
    actual = P.map opOf $ sampleN 5 ops
    expected = [Nop, Nop, Add 2 3 4, Add 2 3 4, Jmp 0x345]


test5 :: Either String ()
test5 = if actual == expected
    then Right ()
    else Left $ P.concat ["Microfetch test 5 failure.\n", "expected: ", show expected, "\nactual: ", show actual]
    where
    ram_contents = (0x1234 :> 0x2000 :> Nil) ++ repeat 0
    mem = ram ram_contents
    (instrs,_) = unbundle $ mem pc 0 (signal NoWrite)
    (pc, ops)  = unbundle $ microfetch empty'' empty' instrs stall jump
    stall = signal False
    jump = fromList [NoJump, NoJump, NoJump, Jump 0x000, NoJump, NoJump]
    actual = P.map opOf $ sampleN 6 ops
    expected = [Nop, Nop, Add 2 3 4, Jmp 0x000, Nop, Add 2 3 4]
    -- This one can be a bit confusing. Just remember that microfetch doesn't have to nop its output; the jump block in D does that.

test6 :: Either String ()
test6 = if actual == expected
    then Right ()
    else Left $ P.concat ["Microfetch test 6 failure.\n", "expected: ", show expected, "\nactual: ", show actual]
    where
    ram_contents = (0x1234 :> 0x5123 :> 0x1789 :> Nil) ++ repeat 0
    mem = ram ram_contents
    (instrs,_) = unbundle $ mem pc 0 (signal NoWrite)
    (pc, ops)  = unbundle $ microfetch empty'' empty' instrs stall jump
    stall = signal False
    jump = signal NoJump
    actual = P.map opOf $ sampleN 7 ops
    expected = [Nop, Nop, Add 2 3 4, Ldr1 1 2, Nop, Ldr2 3, Add 7 8 9]

test7 :: Either String ()
test7 = if actual == expected
    then Right ()
    else Left $ P.concat ["Microfetch test 7 failure.\n", "expected: ", show expected, "\nactual: ", show actual]
    where
    ram_contents = (0x1234 :> 0x5123 :> 0x1789 :> Nil) ++ repeat 0
    mem = ram ram_contents
    (instrs,_) = unbundle $ mem pc 0 (signal NoWrite)
    (pc, ops)  = unbundle $ microfetch empty'' empty' instrs stall jump
    stall = fromList [False, False, False, True, False, False, True, False, False]
    jump = signal NoJump
    actual = P.map opOf $ sampleN 9 ops
    expected = [Nop, Nop, Add 2 3 4, Ldr1 1 2, Ldr1 1 2, Nop, Ldr2 3, Ldr2 3, Add 7 8 9]

-- NB: There should never be a stall during a jump, because the jump block comes first.
test8 :: Either String ()
test8 = if actual == expected
    then Right ()
    else Left $ P.concat ["Microfetch test 8 failure.\n", "expected: ", show expected, "\nactual: ", show actual]
    where
    ram_contents = (0x1234 :> 0x5123 :> 0x1789 :> Nil) ++ repeat 0
    mem = ram ram_contents
    (instrs,_) = unbundle $ mem pc 0 (signal NoWrite)
    (pc, ops)  = unbundle $ microfetch empty'' empty' instrs stall jump
    stall = fromList [False, False, False, True, False, False, False]
    jump = fromList [NoJump, NoJump, NoJump, NoJump, Jump 0x0, NoJump, NoJump]
    actual = P.map opOf $ sampleN 7 ops
    expected = [Nop, Nop, Add 2 3 4, Ldr1 1 2, Ldr1 1 2, Nop, Add 2 3 4]
