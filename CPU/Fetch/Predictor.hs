module CPU.Fetch.Predictor () where

import CLaSH.Prelude
import CPU.Defs(Jump(NoJump), PC)
import qualified CPU.Cache.SetCache as SC

newtype Predictor n m h = Predictor (SC.SetCache n m PC (Markov h))

type Len n = (KnownNat n, KnownNat (2^n))
type NN n = KnownNat n

predict :: (Len n, NN m, Len h, n <= 16) => Predictor n m h -> BitVector h -> PC -> Jump
predict (Predictor sc) jumps pc = case SC.lookup sc pc of
    Nothing     -> NoJump -- We have no markov predictor stored
    Just markov -> markovQuery markov jumps

update :: (Len n, NN m, Len h, n <= 16) => Predictor n m h -> BitVector h -> PC -> Jump -> Predictor n m h
update (Predictor sc) jumps src dst = Predictor $ SC.update update' src (dst, jumps) sc

update' :: Len h => (Jump, BitVector h) -> Maybe (Markov h) -> Maybe (Markov h)
update' (jump, history) old = Just markov'
    where
    markov' = markovWrite history jump $ case old of
        Nothing -> markovEmpty
        Just markov -> markov

newtype Markov h = Markov (Vec (2^h) Jump)

markovWrite :: Len h => BitVector h -> Jump -> Markov h -> Markov h
markovWrite history jump (Markov vec) = Markov $ replace history jump vec

markovEmpty :: Len h => Markov h
markovEmpty = Markov $ repeat NoJump

markovQuery :: Len h => Markov h -> BitVector h -> Jump 
markovQuery (Markov vec) ix = vec !! ix