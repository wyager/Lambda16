module CPU.Rewrite.Microcode (microcode) where

import qualified Prelude as P
import CLaSH.Prelude hiding (init)
import CPU.Defs (S, W(..), PC, Write(..), Jump(..))
import CPU.Ops(Op(..), Fetched(..), invalidated)
import CPU.Fetch.Fetch (fetch)

--type Length = 3

type Ix = BitVector 2

--data Microvec = Microvec (Vec Length Op) (Index Length) deriving Show

rewriteLen :: Fetched -> Ix
rewriteLen fetched = case opOf fetched of
    Ldr _ _ _ -> 3
    _         -> 1

rewrite :: (Ix, Fetched) -> Fetched
rewrite (ix, fetched) = case opOf fetched of
    Ldr a b t -> case ix of
        3 -> Fetched (Ldr1 a b) 0 1  -- If we mispredicted, we still 
        2 -> Fetched Nop        0 1  -- want this to go through.
        1 -> fetched {opOf = Ldr2 t} -- Wait until Ldr2 to detect mispredict.
    _         -> case ix of
        1 -> fetched

next :: (Ix, Fetched) -> (Ix, Fetched)
next (ix, fetched) = (ix - 1, fetched)

init :: Fetched -> (Ix, Fetched)
init fetched = (rewriteLen fetched, fetched)

done :: (Ix, Fetched) -> Bool
done (ix, _) = ix == 0

-- We don't want to rewrite this instruction during a jump. The jmp block will
-- take care of that. We just want to stop stalling.
microcode :: S Bool -> S Fetched -> S Bool -> S (Bool, Fetched)
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

--microcode :: S Bool -> S Op -> S Jump -> S (Bool, Op)
--microcode stall input jmp = bundle (rewriting .||. stall, output)
--    where
--    in_vec = microvec <$> input
--    in_instr = current <$> in_vec
--    output = mux rewriting rewritten in_instr
--    in_vec' = regEn (microvec Nop) (not1 stall) in_vec'''
--    in_vec'' = mux (rewriting .&&. not_jumping) (next <$> in_vec') in_vec -- The next iteration of in_vec
--    in_vec''' = mux not_jumping in_vec'' (signal $ microvec Nop) -- If we're jumping, stop rewriting
--    rewriting = ((not . finished) <$> in_vec')
--    rewritten = (current . next) <$> in_vec'
--    not_jumping = (== NoJump) <$> jmp

--microvec :: Op -> Microvec
--microvec op = case op of
--    Ldr a b t -> Microvec (Ldr2 t :> Nop :> Ldr1 a b :> Nil) (2)
--    op        -> Microvec (op :> Nop :> Nop :> Nil) (0)

--current :: Microvec -> Op
--current (Microvec v i) = v !! i

--next :: Microvec -> Microvec
--next m@(Microvec v i) = Microvec v (i-1)

--finished :: Microvec -> Bool
--finished (Microvec _ 0) = True
--finished _              = False