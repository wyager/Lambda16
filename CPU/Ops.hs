module CPU.Ops (Op(..), parse) where

import CLaSH.Prelude 
import CPU.Defs (W(..), Reg(..), Addr(..), PC(..), Validity(..))

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
          St Reg Addr PC | -- We keep track of the PC so that we know where to jump if we cause a write hazard
          StLit W Addr PC 
          deriving (Eq, Show)

parse :: (W,PC,Validity) -> Op
parse (_,   _,  Invalid) = Nop
parse (W v, pc, Valid)   = case opcode of
    0 -> Mov (W $ zeroExtend $ slice d11 d4 v) t
    1 -> Add a b t
    2 -> Jmp (PC $ zeroExtend $ slice d11 d0 v)
    3 -> Halt
    4 -> Ld (Addr $ zeroExtend $ slice d11 d4 v) t
    5 -> Ldr a b t
    6 -> Jeq a b (pc + (PC $ zeroExtend $ slice d3 d0 v))
    7 -> St a (Addr $ zeroExtend $ slice d7 d0 v) pc
    _ -> error "Invalid opcode"
    where
    opcode = slice d15 d12 v
    a = Reg (slice d11 d8 v)
    b = Reg (slice d7  d4 v)
    t = Reg (slice d3  d0 v)