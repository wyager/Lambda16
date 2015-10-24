{-# LANGUAGE ScopedTypeVariables #-}
module CPU.Ops (Op(..), Fetched(..), invalidated, package, parse, pass) where

import CLaSH.Prelude 
import CPU.Defs (W(..), Reg(..), Addr(..), PC(..), Validity(..), Predicted(..))
import Text.Printf (printf)
import CPU.Safety.Stages (Stage(F,D,R,X), Rep, rep, IsStage, Succ)

data Op = Nop |
          Halt |
          Mov W Reg |
          Add Reg Reg Reg | 
          Jmp PC |
          Jeq Reg Reg PC |             
          Ld Addr Reg | -- Can be replaced with Mov W Reg
          Ldr Reg Reg Reg | 
          Ldr1 Reg Reg |
          Ldr1Lit Addr |
          Ldr2 Reg |
          St Reg Addr | 
          StLit W Addr 
          deriving (Eq, Show)

data Fetched (s :: Stage) = Fetched {opOf :: Op, pcOf :: PC, predictedOf :: Predicted PC}
instance forall s . IsStage s => Show (Fetched s) where 
    show (Fetched op pc pred) = printf "[Fetched@%s [%s] %s %s]" 
      (show (rep :: Rep s))
      (show op)
      (show pc)
      (show pred)

pass :: (IsStage a, IsStage (Succ a)) => Fetched a -> Fetched (Succ a)
pass (Fetched op pc pred) = Fetched op pc pred

invalidated :: IsStage s => Fetched s
invalidated = Fetched Nop 0 1

package :: (Validity, W, PC, Predicted PC) -> Fetched F
package (Invalid, _, _,  _)    = invalidated
package (Valid,   w, pc, pred) = Fetched (parse w) pc pred

parse :: W -> Op
parse (W v) = case opcode of
    0 -> Mov (W $ zeroExtend $ slice d11 d4 v) t
    1 -> Add a b t
    2 -> Jmp (PC $ zeroExtend $ slice d11 d0 v)
    3 -> Halt
    4 -> Ld (Addr $ zeroExtend $ slice d11 d4 v) t
    5 -> Ldr a b t
    6 -> Jeq a b (PC $ zeroExtend $ slice d3 d0 v)
    7 -> St a (Addr $ zeroExtend $ slice d7 d0 v) 
    _ -> Nop -- Not code; ignore.
    where
    opcode = slice d15 d12 v
    a = Reg (slice d11 d8 v)
    b = Reg (slice d7  d4 v)
    t = Reg (slice d3  d0 v)