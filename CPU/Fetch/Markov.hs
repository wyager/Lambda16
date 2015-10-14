module Markov (update, predicted, emptyMarkov) where

import CLaSH.Prelude

data Markov n = Markov (BitVector n) (Vec (2^n) Bool) deriving (Show)

update :: (KnownNat n, KnownNat (2^n)) => Bool -> Markov n -> Markov n
update taken (Markov prev saved) = Markov prev' saved'
    where
    prev' = (prev `shiftL` 1) + if taken then 1 else 0
    saved' = replace prev taken saved

predicted :: (KnownNat n, KnownNat (2^n)) => Markov n -> Bool
predicted (Markov prev saved) = saved !! prev

emptyMarkov :: (KnownNat n, KnownNat (2^n)) => Markov n
emptyMarkov = Markov 0 (repeat False)