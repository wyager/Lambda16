module CPU.Fetch.SimplePredictor (Predictor, PCHash, predictorTap, lookup, empty) where

import CLaSH.Prelude hiding (empty, lookup)
import CPU.Defs (Predicted(..), PC, S)
import CPU.Ops (Fetched, pcOf, predictedOf)
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

lookup :: (PCHash n, KnownNat m) => Predictor n m -> PC -> Predicted PC
lookup (Predictor cache) pc = case SC.lookup cache pc of
    Nothing -> Predicted (pc + 1)
    Just pc -> pc

empty :: (PCHash n, KnownNat m) => Predictor n m
empty = Predictor SC.empty

predictorTap :: (PCHash n, KnownNat m) => S Fetched -> S (Predictor n m)
predictorTap fetched = predictor
    where
    predictor = register empty predictor'
    predictor' = (\fetched predictor -> update predictor (pcOf fetched) (predictedOf fetched)) <$> fetched <*> predictor