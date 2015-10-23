module CPU.Hazard.SelfModifying (selfModifying) where

import CLaSH.Prelude 
import CPU.Defs (Jump(..), PC(..), Addr(..), S)
import CPU.Ops(Op(..), Fetched(..))
import CPU.Safety.Stages (Stage(X))
-- We need to keep track of if any instructions in the pipeline were read from the address currently being written to.
-- If they were, we need to jump to the stored PC + 1.

-- As an aggressive maneuver, we can keep track of the last 4 (?) PCs, including the current one, and if any of them equal 
-- the address being written to, we need to take the jump. Note that we shouldn't include repeat PCs from stalls as new.
-- One way to do this is to just not shift out the old PC unless the new one doesn't match the previous one.

tally :: PC -> Vec 5 PC -> Vec 5 PC
tally pc vec = if head vec == pc
    then vec
    else pc :> init vec

matches :: PC -> Vec 5 PC -> Bool
matches pc vec = foldl (||) False $ map (==pc) vec

selfModifying' :: Fetched X -> Vec 5 PC -> Jump
selfModifying' (Fetched (StLit w (Addr addr)) pc pred) vec = if PC addr `matches` vec
    then Jump (pc + 1)
    else NoJump
selfModifying' _ _ = NoJump

selfModifying :: S PC -> S (Fetched X) -> S Jump
selfModifying pc op =  selfModifying' <$> op <*> pcs -- Using current PC causes loop. I'm just registering after writeback and using a 5-element list. There is probably a better solution. 
    where
    pcs = register (repeat 0) pcs'
    pcs' = tally <$> pc <*> pcs
