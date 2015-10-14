{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Cache.SetCache (SetCache, updateWithKey, update, empty, insert, lookup, delete) where

import CLaSH.Prelude hiding (empty, lookup)
import qualified CPU.Cache.AssocCache as Assoc
import CPU.Cache.Hash (Hash, hash)

newtype SetCache n m k v = SetCache (Vec (2^n) (Assoc.AssocCache m k v)) deriving (Show)

type Len n = (KnownNat n, KnownNat (2^n))

updateWithKey :: forall n m k v v' . (Len n, KnownNat m, Eq k, Hash k n) => (k -> v' -> Maybe v -> Maybe v) -> k -> v' -> SetCache n m k v -> SetCache n m k v
updateWithKey f k v (SetCache assocs) = SetCache $ replace ix assoc' assocs
    where
    ix = hash k :: BitVector n
    assoc = assocs !! ix
    assoc' = Assoc.updateWithKey f k v assoc

update :: (Len n, KnownNat m, Eq k, Hash k n) => (v' -> Maybe v -> Maybe v) -> k -> v' -> SetCache n m k v -> SetCache n m k v
update f = updateWithKey (const f)

insert :: (Len n, KnownNat m, Eq k, Hash k n) => k -> v -> SetCache n m k v -> SetCache n m k v
insert k v = update (\v _ -> Just v) k v

empty :: (Len n, KnownNat m) => SetCache n m k v
empty = SetCache (repeat Assoc.empty)

lookup :: forall n m k v . (Len n, KnownNat m, Eq k, Hash k n) => SetCache n m k v -> k -> Maybe v
lookup (SetCache assocs) k = Assoc.lookup (assocs !! (hash k :: BitVector n)) k

delete :: forall n m k v . (Len n, KnownNat m, Eq k, Hash k n) => k -> SetCache n m k v -> SetCache n m k v
delete k (SetCache assocs) = SetCache $ replace ix assoc' assocs
    where
    ix = hash k :: BitVector n
    assoc = assocs !! ix
    assoc' = Assoc.delete k assoc