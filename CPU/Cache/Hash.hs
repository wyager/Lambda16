{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module CPU.Cache.Hash (Hash, hash) where

import CLaSH.Prelude
import CPU.Defs (PC(..))

class Hash a n where
    hash :: a -> BitVector n

-- The constraint that n (the hash size) is less than b (the number of bits)
-- statically protects us from wasting cache lines that we'll never access
instance (KnownNat n, KnownNat b, n <= b) => Hash (BitVector b) n where
    hash bv = resize bv

instance (KnownNat n, KnownNat b, n <= b) => Hash (Signed b) n where
    hash = resize . pack

instance (KnownNat n, n <= 16) => Hash PC n where
    hash (PC v) = hash v