module CPU.Cache.CacheBlocks (also, regWrites, memWrites, record) where

import CLaSH.Prelude
import CPU.Ops (Op(..), Fetched(..))
import CPU.Defs (S, Reg, Addr, W)
import CPU.Cache.WriteCache as WC (CachedWrite(..), WriteCache, update, empty, lookup)
import CPU.Safety.Stages (IsStage, Stage(X))

regWrites :: IsStage s => Fetched s -> CachedWrite Reg
regWrites fetched = case opOf fetched of
    Mov val reg -> KnownWrite reg val
    Add _ _ reg -> UnknownWrite reg
    Ld _    reg -> UnknownWrite reg
    Ldr2    reg -> UnknownWrite reg
    _           -> NoCachedWrite

memWrites :: IsStage s => Fetched s -> CachedWrite Addr
memWrites fetched = case opOf fetched of
    StLit val addr -> KnownWrite addr val
    St _      addr -> UnknownWrite addr
    _              -> NoCachedWrite

record :: (KnownNat n, Eq a) => (Fetched X -> CachedWrite a) -> S (Fetched X) -> S (WriteCache n a)
record detector x_op = cache'
    where
    cache = register WC.empty cache'
    cache' = (update . detector) <$> x_op <*> cache

also :: (KnownNat n, Eq a, IsStage s) => (Fetched s -> CachedWrite a) -> S (Fetched s) -> S (WriteCache n a) -> S (WriteCache n a)
also detector fetched cache = (update . detector) <$> fetched <*> cache
