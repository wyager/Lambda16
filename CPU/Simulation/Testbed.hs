module CPU.Simulation.Testbed (simulateCPU, loadHexFile, toVector) where

import CLaSH.Prelude
import CPU.Defs (S, W, Read(..), Write(..), Reg)
import CPU.Simulation.RAM (ram)
import CPU.Simulation.Regs (regs)
import Numeric (readHex)
import CPU.CPU (cpu)
import Text.Printf (printf)
import Data.List (intercalate)
import qualified Prelude as P

simulateCPU :: Vec 65536 W -> S String
simulateCPU ram_contents = mux halt (signal "!!!!!!!!!!!!!!! HALT !!!!!!!!!!!!!") $ prettify <$> debug <*> reg_write <*> ram_write
    where
    prettify (sanity, pipeline, dcache, wcache, cache) reg_write mem_write = display [prettify1 reg_write, prettify2 mem_write]-- show sanity, show pipeline, show dcache, show wcache, show cache, prettify1 reg_write]
        where
        display = ("-------------\n" P.++) . intercalate "\n"
        prettify1 (NoWrite) = "Nothing Written"
        prettify1 (Write addr word) = printf "r[0x%x] = 0x%x" (fromEnum addr) (fromEnum word)
        prettify2 (NoWrite) = "No mem write"
        prettify2 (Write addr word) = printf "mem[0x%x] = 0x%x" (fromEnum addr) (fromEnum word)
    ram_read = ram ram_contents ram_pc (toAddr <$> ram_read_addr) ram_write
    reg_read = regs (repeat 0) (toReg <$> reg_read_addr) reg_write 
    (ram_pc, ram_read_addr, reg_read_addr, ram_write, reg_write, halt, debug) = unbundle $ cpu ram_read reg_read
    toAddr (Read addr) = addr
    toAddr _           = 0
    toReg  (Read regs) = regs
    toReg  _           = (0,0) 

loadHexFile :: (Eq a, Num a) => String -> IO [a]
loadHexFile path = do
    hexes <- lines <$> readFile path
    return $ P.map (fst . P.head . readHex) hexes

toVector :: KnownNat n => [a] -> Vec n a
toVector items = map P.head $ iterateI P.tail items