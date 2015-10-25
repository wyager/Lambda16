module CPU.Detector.SanityBlock (Sanity(..), dSanity, rSanity, xSanity) where

import CLaSH.Prelude
import Data.Monoid (Monoid, mempty, mappend)
import CPU.Ops(Op(..), Fetched(..))
import CPU.Defs(prediction)
import CPU.Safety.Stages (Stage(F,D,R,X))

data Sanity = Sane | Insane (BitVector 16) deriving Show

instance Monoid Sanity where
    mempty = Sane
    mappend (Insane a) (Insane b) = Insane (a .|. b)
    mappend (Insane a) Sane = Insane a
    mappend Sane (Insane b) = Insane b
    mappend Sane Sane = Sane

insane :: Int -> Sanity
insane x = Insane (shiftL 1 x)

dSanity :: Fetched D -> Sanity
dSanity d_op = case opOf d_op of
    Ldr _ _ _ -> insane 0
    Ldr2 _    -> insane 1
    Jmp pc    -> if prediction (predictedOf d_op) /= pc
                    then insane 2
                    else Sane
    _         -> Sane


rSanity :: Fetched R -> Sanity
rSanity r_op = case opOf r_op of
    Ldr _ _ _ -> insane 3
    Ldr2 _    -> insane 4
    Jmp pc    -> if prediction (predictedOf r_op) /= pc
                    then insane 5
                    else Sane
    _         -> Sane


xSanity :: Fetched X -> Sanity
xSanity x_op = case opOf x_op of
    Ldr _ _ _ -> insane 6
    Ldr1 _ _  -> insane 7
    Ldr2 _    -> insane 8
    Jmp pc    -> if prediction (predictedOf x_op) /= pc
                    then insane 9
                    else Sane
    Add _ _ _ -> insane 10
    Jeq _ _ _ -> insane 11
    St _ _    -> insane 12
    _         -> Sane