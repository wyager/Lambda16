module CPU.Rewrite.Microcode (microcode) where

import qualified Prelude as P
import CLaSH.Prelude
import CPU.Defs (S, W(..), PC, Write(..), Jump(..))
import CPU.Ops(Op(..), parse)
import CPU.Fetch.Fetch (fetch)

data Microvec = Microvec (Vec 3 Op) (Index 3) deriving Show

microcode :: S Bool -> S Op -> S Jump -> S (Bool, Op)
microcode stall input jmp = bundle (rewriting .||. stall, output)
    where
    in_vec = microvec <$> input
    in_instr = current <$> in_vec
    output = mux rewriting rewritten in_instr
    in_vec' = regEn (microvec Nop) (not1 stall) in_vec'''
    in_vec'' = mux (rewriting .&&. not_jumping) (next <$> in_vec') in_vec -- The next iteration of in_vec
    in_vec''' = mux not_jumping in_vec'' (signal $ microvec Nop) -- If we're jumping, stop rewriting
    rewriting = ((not . finished) <$> in_vec')
    rewritten = (current . next) <$> in_vec'
    not_jumping = (== NoJump) <$> jmp

microvec :: Op -> Microvec
microvec op = case op of
    Ldr a b t -> Microvec (Ldr2 t :> Nop :> Ldr1 a b :> Nil) (2)
    op        -> Microvec (op :> Nop :> Nop :> Nil) (0)

current :: Microvec -> Op
current (Microvec v i) = v !! i

next :: Microvec -> Microvec
next m@(Microvec v i) = Microvec v (i-1)

finished :: Microvec -> Bool
finished (Microvec _ 0) = True
finished _              = False