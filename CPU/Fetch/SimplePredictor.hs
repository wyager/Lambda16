module SimplePredictor () where

import CLaSH.Prelude
import CPU.Defs (Predicted(..), PC)
import qualified CPU.Cache.SetCache as SC

type PCHash n = (KnownNat n, KnownNat (2^n), n <= 16)

newtype Predictor n m = Predictor (SC.SetCache n m PC (Predicted PC))

-- Bug? Try replacing these constraints with a type hole. It infers no constraints.
update :: (PCHash n, KnownNat m) => Predictor n m -> PC -> Predicted PC -> Predictor n m
update predictor start pred@(Predicted end) = if end == start + 1
    then delete start      predictor
    else insert start pred predictor

delete :: (PCHash n, KnownNat m) => PC -> Predictor n m -> Predictor n m
delete pc (Predictor cache) = Predictor $ SC.delete pc cache

insert :: (PCHash n, KnownNat m) => PC -> Predicted PC -> Predictor n m -> Predictor n m
insert pc pred (Predictor cache) = Predictor $ SC.insert pc pred cache

lookup :: (PCHash n, KnownNat m) => Predictor n m -> PC -> Maybe (Predicted PC)
lookup (Predictor cache) pc = SC.lookup cache pc