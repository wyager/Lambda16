module CPU.Cache.AssocCache (AssocCache, update, updateWithKey, insert, empty, lookup, delete) where -- (Cache, CachedWrite(..), cache, emptyCache, lookup, unknownWritesTo, writesTo) where

import CLaSH.Prelude hiding (lookup, empty)
import Data.Maybe (isNothing)

newtype AssocCache n k v = AssocCache (Vec n (Maybe (k,v))) deriving (Show)

empty :: KnownNat n => AssocCache n k v
empty = AssocCache (repeat Nothing)

lookup :: (KnownNat n, Eq k) => AssocCache n k v -> k -> Maybe v
lookup (AssocCache entries) k = foldl (<|>) Nothing $ map (matches k) entries
    where 
    matches k (Just (k',v)) | k == k' = Just v
    matches _ _                       = Nothing

updateWithKey :: (KnownNat n, Eq k) => (k -> v' -> Maybe v -> Maybe v) -> k -> v' -> AssocCache n k v -> AssocCache n k v
updateWithKey f k v cache = case f k v (lookup cache k) of
    Nothing -> bunched
    Just v' -> prepend (Just (k,v')) $ bunched
    where
    bunched = bunch $ delete k cache 

update :: (KnownNat n, Eq k) => (v' -> Maybe v -> Maybe v) -> k -> v' -> AssocCache n k v -> AssocCache n k v
update f = updateWithKey (const f)

insert :: (KnownNat n, Eq k) => k -> v -> AssocCache n k v -> AssocCache n k v
insert k v = prepend (Just (k,v)) . bunch . delete k

-- Remove the left-most empty slot and slide everything after it over one
bunch :: (KnownNat n) => AssocCache n k v -> AssocCache n k v
bunch (AssocCache vec) = AssocCache vec'
    where
    empty = map isNothing vec
    free = postscanl (||) False empty
    shifted = vec <<+ Nothing
    vec' = zipWith3 (\free unslid slid -> if free then slid else unslid) free vec shifted

delete :: (KnownNat n, Eq k) => k -> AssocCache n k v -> AssocCache n k v
delete a (AssocCache entries) = AssocCache (map delete' entries)
    where
    delete' Nothing = Nothing
    delete' (Just (a_old, w)) = if a == a_old
        then Nothing
        else Just (a_old, w)

prepend :: (KnownNat n) => Maybe (k,v) -> AssocCache n k v -> AssocCache n k v
prepend v (AssocCache entries) = AssocCache (v +>> entries)
