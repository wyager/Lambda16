module CPU.Fetch.Fetch (fetch) where

import qualified Prelude as P
import CLaSH.Prelude hiding (lookup)
import CPU.Defs (S, W(..), PC, Write(..), Jump(..), Validity(..), Predicted(..))
import CPU.Fetch.SimplePredictor (PCHash, Predictor, lookup, predictorTap)
import CPU.Ops(Fetched)
import CPU.Safety.Stages (Stage(X))
-- import RAM (ram)

-- Stall means to hold the current state
fetch :: S (Fetched X) -> S W -> S Bool -> S Jump -> S (PC, (Validity, W, PC, Predicted PC))
fetch x_op mem_read stall jmp = bundle (pc', bundle (f1_valid, instr, f1_pc, f1_predicted))
    where
    not_stall = not <$> stall
    instr = mux stalled prev mem_read
    prev = register 0xF0F0 instr
    stalled = register False stall

    f1_pc = regEn undefined not_stall f_pc
    f_pc = register (-1) pc'

    f1_valid = regEn Invalid not_stall (mux jumping (signal Invalid) f_valid)
    f_valid = register Invalid (signal Valid)

    f1_predicted = regEn undefined not_stall f_predicted
    f_predicted = register 0 $ lookup <$> predictor <*> pc'

    predictor = predictorTap x_op :: S (Predictor 3 4)

    f_pc_pred = prediction <$> (lookup <$> predictor <*> f_pc)
    pc' = jump <$> jmp <*> mux stall f_pc (prediction <$> f_predicted)
    jump (NoJump) pc = pc
    jump (Jump pc) _ = pc
    jumping = (/= NoJump) <$> jmp