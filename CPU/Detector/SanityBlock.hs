module CPU.Detector.SanityBlock (Sanity(..), decodeSanity, waitSanity, writebackSanity) where

import CLaSH.Prelude
import Data.Monoid (Monoid, mempty, mappend)
import CPU.Ops(Op(..), Fetched(..))
import CPU.Defs(prediction)

data Sanity = Sane | Insane (BitVector 16) deriving Show

instance Monoid Sanity where
    mempty = Sane
    mappend (Insane a) (Insane b) = Insane (a .|. b)
    mappend (Insane a) Sane = Insane a
    mappend Sane (Insane b) = Insane b
    mappend Sane Sane = Sane

insane :: Int -> Sanity
insane x = Insane (shiftL 1 x)

decodeSanity :: Fetched -> Sanity
decodeSanity fetched = case opOf fetched of
    Ldr _ _ _ -> insane 0
    Ldr2 _    -> insane 1
    Jmp pc    -> if prediction (predictedOf fetched) /= pc
                    then insane 2
                    else Sane
    _         -> Sane


waitSanity :: Fetched -> Sanity
waitSanity fetched = case opOf fetched of
    Ldr _ _ _ -> insane 3
    Ldr2 _    -> insane 4
    Jmp pc    -> if prediction (predictedOf fetched) /= pc
                    then insane 5
                    else Sane
    _         -> Sane


writebackSanity :: Fetched -> Sanity
writebackSanity fetched = case opOf fetched of
    Ldr _ _ _ -> insane 6
    Ldr1 _ _  -> insane 7
    Ldr2 _    -> insane 8
    Jmp pc    -> if prediction (predictedOf fetched) /= pc
                    then insane 9
                    else Sane
    Add _ _ _ -> insane 10
    Jeq _ _ _ -> insane 11
    St _ _    -> insane 12
    _         -> Sane