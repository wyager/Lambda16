module CPU.Cache.CacheBlocks (Tap, record, also, regWrites, memWrites, tap, also') where

import CLaSH.Prelude
import CPU.Ops (Op(..), Fetched(..))
import CPU.Defs (S, Reg, Addr, W)
import CPU.Cache.WriteCache as WC (CachedWrite(..), WriteCache, update, empty, lookup)

type Tap a = S a -> S (Maybe W)

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

tap :: (KnownNat n, Eq a) => S (WriteCache n a) -> Tap a
tap = liftA2 WC.lookup

also' :: Eq a => (Fetched -> CachedWrite a) -> S Fetched -> Tap a -> Tap a
also' detector fetched search addr = also'' <$> prevResult <*> write <*> addr
    where
    prevResult = search addr :: S (Maybe W)
    write = detector <$> fetched
    also'' prev NoCachedWrite    _    = prev
    also'' prev (UnknownWrite a) addr = if a == addr then Nothing else prev
    also'' prev (KnownWrite a v) addr = if a == addr then Just v  else prev

