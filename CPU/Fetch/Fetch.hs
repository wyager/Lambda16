module CPU.Fetch.Fetch (fetch) where

import qualified Prelude as P
import CLaSH.Prelude
import CPU.Defs (S, W(..), PC, Write(..), Jump(..), Validity(..), Predicted(..))
-- import RAM (ram)

-- Stall means to hold the current state
fetch :: S W -> S Bool -> S Jump -> S (PC, (Validity, W, PC, Predicted PC))
fetch mem_read stall jmp = bundle (pc', bundle (f1_valid, instr, f1_pc, f1_predicted))
    where
    instr = mux stalled prev mem_read
    prev = register 0xF0F0 instr
    stalled = register False stall
    f1_pc = register undefined (mux stall f1_pc f_pc)
    f_pc = register (-1) pc'

    f1_valid = regEn Invalid (not1 stall) (mux jumping (signal Invalid) f_valid)
    f_valid = regEn Invalid (not1 stall) (signal Valid)

    f1_predicted = register undefined f_predicted
    f_predicted = Predicted <$> f_pc + 1

    pc' = jump <$> jmp <*> mux stall f_pc (prediction <$> f_predicted)
    jump (NoJump) pc = pc
    jump (Jump pc) _ = pc
    jumping = (/= NoJump) <$> jmp