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
    mapM (\(num,op) -> putStrLn $ show num P.++ ": " P.++ show op) $ P.zip [0..] $ P.map parse mem
    putStrLn "\n\n------Output:-------"
    mapM putStrLn $ sampleN 10000 $ simulateCPU ram
    