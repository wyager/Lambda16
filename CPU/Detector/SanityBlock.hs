module CPU.Detector.SanityBlock (Sanity(..), decodeSanity, waitSanity, writebackSanity) where

import CLaSH.Prelude
import Data.Monoid (Monoid, mempty, mappend)
import CPU.Ops(Op(..))

data Sanity = Sane | Insane (BitVector 16) deriving Show

instance Monoid Sanity where
    mempty = Sane
    mappend (Insane a) (Insane b) = Insane (a .|. b)
    mappend (Insane a) Sane = Insane a
    mappend Sane (Insane b) = Insane b
    mappend Sane Sane = Sane

insane :: Int -> Sanity
insane x = Insane (shiftL 1 x)

decodeSanity :: Op -> Sanity
decodeSanity op = case op of
    Ldr _ _ _ -> insane 0
    Ldr2 _    -> insane 1
    Jmp _     -> insane 2
    _         -> Sane


waitSanity :: Op -> Sanity
waitSanity op = case op of
    Ldr _ _ _ -> insane 3
    Ldr2 _    -> insane 4
    Jmp _     -> insane 5
    _         -> Sane


writebackSanity :: Op -> Sanity
writebackSanity op = case op of
    Ldr _ _ _ -> insane 6
    Ldr1 _ _  -> insane 7
    Ldr2 _    -> insane 8
    Jmp _     -> insane 9
    Add _ _ _ -> insane 10
    Jeq _ _ _ -> insane 11
    St _ _ _  -> insane 12
    _         -> Sane