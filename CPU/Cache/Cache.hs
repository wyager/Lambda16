module CPU.Cache.Cache (Cache, CachedWrite(..), update, emptyCache, lookup) where -- (Cache, CachedWrite(..), cache, emptyCache, lookup, unknownWritesTo, writesTo) where

import CLaSH.Prelude hiding (lookup)
import CPU.Defs (W, S)

newtype Cache n a = Cache (Vec n (Maybe (a,W))) deriving (Show)

data CachedWrite a = NoCachedWrite | UnknownWrite a | KnownWrite a W deriving (Show, Eq)

insert :: (KnownNat n, Eq a) => CachedWrite a -> Cache n a -> Cache (n+1) a
insert NoCachedWrite cache = prepend Nothing cache
insert (UnknownWrite a) cache = prepend Nothing $ delete a cache
insert (KnownWrite a w) cache = prepend (Just (a,w)) $ delete a cache

-- Delete oldest entry *only* if there's a new write
update :: (KnownNat n, Eq a) => CachedWrite a -> Cache n a -> Cache n a
update NoCachedWrite cache = cache
update (UnknownWrite a) cache = delete a cache
update (KnownWrite a w) cache = deleteOldest $ prepend (Just (a,w)) $ bunch $ delete a cache

-- Remove the left-most empty slot and slide everything after it over one
bunch :: (KnownNat n, Eq a) => Cache n a -> Cache n a
bunch (Cache vec) = Cache vec'
    where
    empty = map (==Nothing) vec
    free = sscanl (||) False empty
    shifted = tail (vec <: Nothing)
    vec' = zipWith3 (\free unslid slid -> if free then slid else unslid) free vec shifted

delete :: (KnownNat n, Eq a) =>  a -> Cache n a -> Cache n a
delete a (Cache entries) = Cache (map delete' entries)
    where
    delete' Nothing = Nothing
    delete' (Just (a_old, w)) = if a == a_old
        then Nothing
        else Just (a_old, w)

prepend :: (KnownNat n, Eq a) => (Maybe (a,W)) -> Cache n a -> Cache (n + 1) a
prepend v (Cache entries) = Cache (v :> entries)

deleteOldest :: (KnownNat n, Eq a) => Cache (n+1) a -> Cache n a
deleteOldest (Cache entries) = Cache (init entries)

lookup :: (KnownNat n, Eq a) => Cache n a -> a -> Maybe W
lookup (Cache entries) a = foldl (<|>) Nothing $ map (matches a) entries
    where 
    matches a (Just (a',w)) | a == a' = Just w
    matches _ _                       = Nothing

emptyCache :: KnownNat n => Cache n a
emptyCache = Cache (repeat Nothing)
