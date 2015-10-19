module CPU.Fetch.Fetch (fetch) where

import qualified Prelude as P
import CLaSH.Prelude hiding (lookup)
import CPU.Defs (S, W(..), PC, Write(..), Jump(..), Validity(..), Predicted(..))
import CPU.Fetch.SimplePredictor (PCHash, Predictor, lookup)
-- import RAM (ram)

-- Stall means to hold the current state
fetch :: (PCHash j, KnownNat k) => S (Predictor j k) -> S W -> S Bool -> S Jump -> S (PC, (Validity, W, PC, Predicted PC))
fetch predictor mem_read stall jmp = bundle (pc', bundle (f1_valid, instr, f1_pc, f1_predicted))
    where
    instr = mux stalled prev mem_read
    prev = register 0xF0F0 instr
    stalled = register False stall
    f1_pc = register undefined (mux stall f1_pc f_pc)
    f_pc = register (-1) pc'

    f1_valid = regEn Invalid (not1 stall) (mux jumping (signal Invalid) f_valid)
    f_valid = regEn Invalid (not1 stall) (signal Valid)

    f1_predicted = register undefined f_predicted
    f_predicted = lookup <$> predictor <*> f_pc
    --prev_predictor = register undefined predictor'
    --predictor' = mux stalled prev_predictor predictor

    pc' = jump <$> jmp <*> mux stall f_pc (prediction <$> f_predicted)
    jump (NoJump) pc = pc
    jump (Jump pc) _ = pc
    jumping = (/= NoJump) <$> jmp