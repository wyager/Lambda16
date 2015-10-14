module CPU.Cache.WriteCache (WriteCache, CachedWrite(..), update, empty, lookup) where -- (Cache, CachedWrite(..), cache, emptyCache, lookup, unknownWritesTo, writesTo) where

import CLaSH.Prelude hiding (lookup, empty)
import qualified CPU.Cache.AssocCache as Assoc
import CPU.Defs (W, S)

newtype WriteCache n a = WC (Assoc.AssocCache n a W) deriving (Show)

data CachedWrite a = NoCachedWrite | UnknownWrite a | KnownWrite a W deriving (Show, Eq)

-- Delete oldest entry *only* if there's a new write
update :: (KnownNat n, Eq a) => CachedWrite a -> WriteCache n a -> WriteCache n a
update NoCachedWrite cache = cache
update (UnknownWrite a) (WC cache) = WC $ Assoc.delete a   cache
update (KnownWrite a w) (WC cache) = WC $ Assoc.insert a w cache

lookup :: (KnownNat n, Eq a) => WriteCache n a -> a -> Maybe W
lookup (WC entries) a = Assoc.lookup entries a

empty :: KnownNat n => WriteCache n a
empty = WC $ Assoc.empty
