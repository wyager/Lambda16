module CPU.CPU (cpu) where

import CLaSH.Prelude hiding (Read)
import CPU.Defs(S,W,PC,Reg,Write(..),Read,Addr,Jump(..))
import CPU.Ops(Op(..), invalidated)
import CPU.Fetch.MicroFetch (microfetch)
import CPU.Rewrite.RewriteBlocks (decodeRewrite, waitRewrite, writebackRewrite)
import CPU.Detector.ReadBlocks (memReadBlock, regReadBlock)
import CPU.Detector.WriteBlocks (memWritebackBlock, regWritebackBlock)
import CPU.Detector.HaltBlock (haltBlock)
import CPU.Cache.WriteCache (WriteCache)
import CPU.Cache.CacheBlocks (record, also, regWrites, memWrites)
import CPU.Detector.SanityBlock (Sanity(..), decodeSanity, waitSanity, writebackSanity)
import CPU.Simulation.Pipeline (Pipeline(..))
import CPU.Hazard.SelfModifying (selfModifying)

import Data.Monoid (Monoid, (<>))

type Debug = (Sanity, Pipeline, WriteCache 8 Reg, WriteCache 8 Reg, WriteCache 8 Reg)


-- Roughly speaking:
--    ^--RAM--v            ^------(memRead, regRead)---------v       ^-------(memWriteback, regWriteback, halt)            
-- fetch <> microcode |> decodeRewrite |> waitRewrite |> writebackRewrite |> writeback' (pending writes)
--                                          ^cache-----------------------v-------------v
--                          ^cache----------v--------v

cpu :: S (W,W) -> S (W,W) -> S (PC, Read Addr, Read (Reg, Reg), Write Addr, Write Reg, Bool, Debug)
cpu mem regs = bundle (mem_read_pc, mem_read, reg_read, mem_write, reg_write, halt, debug)
    where
    (mem_read_1, mem_read_2) = unbundle mem
    (reg_read_1, reg_read_2) = unbundle regs
    (mem_read_pc, fetch_op)  = unbundle $ microfetch decode_cache mem_read_1 stall decode_jump
    (stall, decode_jump, decode_op) = unbundle $ decodeRewrite <$> wait_op <*> writeback_op <*> decode_mem_cache <*> decode_cache <*> wait_jump <*> fetch_op
    mem_read = memReadBlock <$> decode_op
    reg_read = regReadBlock <$> decode_op
    decode_op' = register invalidated decode_op
    (wait_jump, wait_op) = unbundle $ waitRewrite <$> wait_mem_cache <*> wait_cache <*> writeback_jump <*> decode_op'
    wait_op' = register invalidated wait_op
    (writeback_jump, writeback_op) = unbundle $ writebackRewrite <$> mem_read_2 <*> regs <*> mem_hazard_jump <*> wait_op'
    mem_write = register NoWrite $ memWritebackBlock <$> writeback_op
    reg_write = regWritebackBlock <$> writeback_op
    halt = haltBlock <$> writeback_op
    wb' = register invalidated writeback_op
    mem_hazard_jump = selfModifying mem_read_pc wb'

    decode_cache = also regWrites wait_op wait_cache
    wait_cache = also regWrites writeback_op cache
    cache = record regWrites writeback_op :: S (WriteCache 8 Reg)

    decode_mem_cache = also memWrites wait_op wait_mem_cache
    wait_mem_cache = also memWrites writeback_op mem_cache
    mem_cache = record memWrites writeback_op :: S (WriteCache 8 Addr)

    sanity = (decodeSanity <$> decode_op) <<>> (waitSanity <$> wait_op) <<>> (writebackSanity <$> writeback_op)

    debug = bundle (sanity, pipeline, decode_cache, wait_cache, cache)
    pipeline = Pipeline <$> fetch_op <*> decode_op <*> decode_op' <*> wait_op <*> wait_op' <*> writeback_op

(<<>>) :: (Monoid m) => S m -> S m -> S m
(<<>>) = liftA2 (<>)
