module CPU.Cache.CacheBlocks (record, also, regWrites, memWrites) where

import CLaSH.Prelude
import CPU.Ops (Op(..), Fetched(..))
import CPU.Defs (S, Reg, Addr)
import CPU.Cache.WriteCache as WC (CachedWrite(..), WriteCache, update, empty)

regWrites :: Fetched -> CachedWrite Reg
regWrites fetched = case opOf fetched of
    Mov val reg -> KnownWrite reg val
    Add _ _ reg -> UnknownWrite reg
    Ld _    reg -> UnknownWrite reg
    Ldr2    reg -> UnknownWrite reg
    _           -> NoCachedWrite

memWrites :: Fetched -> CachedWrite Addr
memWrites fetched = case opOf fetched of
    StLit val addr -> KnownWrite addr val
    St _      addr -> UnknownWrite addr
    _              -> NoCachedWrite

record :: (KnownNat n, Eq a) => (Fetched -> CachedWrite a) -> S Fetched -> S (WriteCache n a)
record detector = moore update' id WC.empty
    where
    update' cache fetched = update (detector fetched) cache

also :: (KnownNat n, Eq a) => (Fetched -> CachedWrite a) -> S Fetched -> S (WriteCache n a) -> S (WriteCache n a)
also detector fetched cache = (update . detector) <$> fetched <*> cache

--tap :: (KnownNat n, Eq a) => S (WriteCache n a) -> S a -> S (Maybe W)
--tap = liftA2 WC.lookup