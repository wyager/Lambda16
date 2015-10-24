module CPU.CPU (cpu) where

import CLaSH.Prelude hiding (Read)
import CPU.Defs(S,W,PC,Reg,Write(..),Read,Addr,Jump(..))
import CPU.Ops(Op(..), invalidated, pass)
import CPU.Fetch.MicroFetch (microfetch)
import CPU.Rewrite.RewriteBlocks (decodeRewrite, waitRewrite, writebackRewrite)
import CPU.Detector.ReadBlocks (memReadBlock, regReadBlock)
import CPU.Detector.WriteBlocks (memWritebackBlock, regWritebackBlock)
import CPU.Detector.HaltBlock (haltBlock)
import CPU.Cache.WriteCache (WriteCache)
import CPU.Cache.CacheBlocks (record, also, regWrites, memWrites, Tap, tap, also')
import CPU.Detector.SanityBlock (Sanity(..), decodeSanity, waitSanity, writebackSanity)
import CPU.Simulation.Pipeline (Pipeline(..))
import CPU.Hazard.SelfModifying (selfModifying)
import CPU.Fetch.SimplePredictor (Predictor, predictorTap)
import Data.Monoid (Monoid, (<>))

type Debug = () 
--type Debug = (Sanity, Pipeline, WriteCache 8 Reg, WriteCache 8 Reg, WriteCache 8 Reg, Jump, (W,W))

cpu :: S (W,W) -> S (W,W) -> S (PC, Read Addr, Read (Reg, Reg), Write Addr, Write Reg, Bool, Debug)
cpu mem regs = bundle (mem_read_pc, mem_read, reg_read, mem_write, reg_write, halt, debug)
    where
    (nem_data_1, mem_data_2) = unbundle mem
    (mem_read_pc, f_op)  = unbundle $ microfetch x_op d_cache nem_data_1 stall d_jump
    (stall, d_jump, d_op) = unbundle $ decodeRewrite <$> r_op <*> x_op <*> d_mem_cache <*> d_cache <*> r_jump <*> (pass <$> f_op)
    mem_read = memReadBlock <$> d_op
    reg_read = regReadBlock <$> d_op
    d_op' = register invalidated d_op
    (r_jump, r_op) = unbundle $ waitRewrite <$> r_mem_cache <*> r_cache <*> writeback_jump <*> (pass <$> d_op')
    r_op' = register invalidated r_op
    (writeback_jump, x_op) = unbundle $ writebackRewrite <$> mem_data_2 <*> regs <*> mem_hazard_jump <*> (pass <$> r_op')
    mem_write = register NoWrite $ memWritebackBlock <$> x_op
    reg_write = regWritebackBlock <$> x_op
    halt = haltBlock <$> x_op
    x_op' = register invalidated x_op
    mem_hazard_jump = selfModifying mem_read_pc x_op'

    -- Reg write caches
    d_cache = also regWrites r_op r_cache   :: S (WriteCache 8 Reg)
    r_cache = also regWrites x_op cache     :: S (WriteCache 8 Reg)
    cache = record regWrites x_op           :: S (WriteCache 8 Reg)
    cache_tap = tap cache :: Tap Reg
    r_tap = also' regWrites x_op cache_tap
    d_tap = also' regWrites r_op r_tap

    -- Mem write caches
    d_mem_cache = also memWrites r_op r_mem_cache   :: S (WriteCache 8 Addr)
    r_mem_cache = also memWrites x_op mem_cache     :: S (WriteCache 8 Addr)
    mem_cache = record memWrites x_op               :: S (WriteCache 8 Addr)

    debug = signal ()
    --debug = bundle (sanity, pipeline, d_cache, r_cache, cache, d_jump, regs)
    --pipeline = Pipeline <$> f_op <*> d_op <*> d_op' <*> r_op <*> r_op' <*> x_op
    --sanity = (decodeSanity <$> d_op) <<>> (waitSanity <$> r_op) <<>> (writebackSanity <$> x_op)

(<<>>) :: (Monoid m) => S m -> S m -> S m
(<<>>) = liftA2 (<>)
