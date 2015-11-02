module CPU.Rewrite.Microcode (microcode) where

import qualified Prelude as P
import CLaSH.Prelude hiding (init)
import CPU.Defs (S, W(..), PC, Write(..), Jump(..))
import CPU.Ops(Op(..), Fetched(..), invalidated)
import CPU.Fetch.Fetch (fetch)
import CPU.Safety.Stages (Stage(F))

type Ix = BitVector 2

rewriteLen :: Fetched F -> Ix
rewriteLen fetched = case opOf fetched of
    Ldr _ _ _ -> 3
    _         -> 1

rewrite :: (Ix, Fetched F) -> Fetched F
rewrite (ix, fetched) = case opOf fetched of
    Ldr a b t -> case ix of
        3 -> Fetched (Ldr1 a b) 0 1  -- If we mispredicted, we still 
        2 -> Fetched Nop        0 1  -- want this to go through.
        1 -> fetched {opOf = Ldr2 t} -- Wait until Ldr2 to detect mispredict.
    _         -> case ix of
        1 -> fetched

next :: (Ix, Fetched F) -> (Ix, Fetched F)
next (ix, fetched) = (ix - 1, fetched)

init :: Fetched F -> (Ix, Fetched F)
init fetched = (rewriteLen fetched, fetched)

done :: (Ix, Fetched F) -> Bool
done (ix, _) = ix == 0

-- We don't want to rewrite this instruction during a jump. The jmp block will
-- take care of that. We just want to stop stalling.
microcode :: S Bool -> S (Fetched F) -> S Bool -> S (Bool, Fetched F)
microcode stall fetched jumping = bundle (fetch_stall, out_fetched)
    where
    input = init <$> fetched
    current = mux rewriting prev_succ input 
    -- If we're jumping, don't keep rewriting the next cycle
    succ = mux jumping (signal $ init invalidated) $ next <$> current
    will_rewrite = (not . done) <$> succ
    prev_succ = regEn (init invalidated) (not <$> stall) succ
    rewriting = (not . done) <$> prev_succ
    out_fetched = rewrite <$> current
    fetch_stall = will_rewrite .||. stall
