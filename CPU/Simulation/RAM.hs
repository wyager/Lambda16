module CPU.Simulation.RAM (ram, test) where

import qualified Prelude as P
import CLaSH.Prelude
import CLaSH.Sized.Vector

import CPU.Defs (S, PC, Addr, W(..), Write(..))

ram :: Vec 65536 W -> S PC -> S Addr -> S (Write Addr) -> S (W, W)
ram initial pc addr write = bundle (pc'', addr'')
    where
    memory = register initial (modify <$> memory <*> write)
    modify mem (Write addr word) = replace addr word mem
    modify mem NoWrite           = mem
    pc' = register 0xF0F0 pc
    pc'' = register 0xF0F0 (liftA2 (!!) memory pc')
    addr' = register 0xF0F0 addr
    addr'' = register 0xF0F0 (liftA2 (!!) memory addr')

test :: ()
test = if expected == (P.tail $ P.tail $ actual)
    then ()
    else error ("Test failed:" P.++ show actual) 
    where
    contents = W <$> iterateI (+1) 0 
    pcreads = fromList [1,2,3,4,5,5]:: S PC
    adreads = fromList [5,6,7,8,5,5] :: S Addr
    writes  = fromList [NoWrite, NoWrite, NoWrite, NoWrite, Write 5 0xF0, Write 5 0x0F]
    mem = ram contents
    output = mem pcreads adreads writes
    actual = sampleN 8 output
    expected = [(1,5),(2,6),(3,7),(4,8),(0xF0,0xF0),(0xF,0xF)]
