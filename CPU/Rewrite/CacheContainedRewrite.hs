module CPU.Rewrite.CacheContainedRewrite (dRewrite') where

import CLaSH.Prelude
import CPU.Defs (S, Jump, Reg, Addr)
import CPU.Ops (Fetched)
import CPU.Safety.Stages (Stage(D,R,X))
import CPU.Rewrite.RewriteBlocks (microfetchRewrite, dRewrite, rRewrite, xRewrite)
import CPU.Cache.WriteCache (WriteCache, empty)
import CPU.Cache.CacheBlocks (record, also, regWrites, memWrites)

dRewrite' :: S (Fetched R) -> S (Fetched X) -> S Jump -> S (Fetched D) -> S (Bool, Jump, Fetched D)
dRewrite' r_op x_op jump d_op = dRewrite <$> r_op <*> x_op <*> mem_cache <*> reg_cache <*> jump <*> d_op
    where
    mem_cache = also memWrites r_op $ also memWrites x_op $ record memWrites x_op :: S (WriteCache 8 Addr)
    reg_cache = also regWrites r_op $ also regWrites x_op $ record regWrites x_op :: S (WriteCache 8 Reg)