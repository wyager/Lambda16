import qualified CPU.Fetch.MicroFetch as MF
import CLaSH.Prelude
import CPU.CPU (cpu)
import CPU.Simulation.Testbed (simulateCPU, loadHexFile, toVector)
import CPU.Defs (W, Validity(Valid))
import CPU.Ops(parse)
import qualified Prelude as P

main = do
    mem <- loadHexFile "mem.hex"
    let ram = toVector mem :: Vec 65536 W
    putStrLn "\n-----Loaded RAM-----"
    putStrLn "------Program:------\n"
    mapM print $ P.map parse $ P.zip3 mem [0..] (P.repeat Valid)
    putStrLn "\n\n------Output:-------"
    mapM putStrLn $ sampleN 130 $ simulateCPU ram
    