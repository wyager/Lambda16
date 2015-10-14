module CPU.Cache.CacheBlocks (record, also, regWrites, memWrites) where

import CLaSH.Prelude
import CPU.Ops (Op(..))
import CPU.Defs (S, Reg, Addr)
import CPU.Cache.WriteCache as WC (CachedWrite(..), WriteCache, update, empty)

regWrites :: Op -> CachedWrite Reg
regWrites op = case op of
    Mov val reg -> KnownWrite reg val
    Add _ _ reg -> UnknownWrite reg
    Ld _    reg -> UnknownWrite reg
    Ldr2    reg -> UnknownWrite reg
    _           -> NoCachedWrite

memWrites :: Op -> CachedWrite Addr
memWrites op = case op of
    StLit val addr _ -> KnownWrite addr val
    St _      addr _ -> UnknownWrite addr
    _                -> NoCachedWrite

record :: (KnownNat n, Eq a) => (Op -> CachedWrite a) -> S Op -> S (WriteCache n a)
record detector = moore update' id WC.empty
    where
    update' cache op = update (detector op) cache

also :: (KnownNat n, Eq a) => (Op -> CachedWrite a) -> S Op -> S (WriteCache n a) -> S (WriteCache n a)
also detector op cache = (update . detector) <$> op <*> cache