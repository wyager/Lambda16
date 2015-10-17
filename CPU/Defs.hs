{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CPU.Defs (S,Addr(..),W(..),Reg(..),PC(..),Write(..),Read(..),Jump(..),Validity(..), Predicted(..)) where

import CLaSH.Prelude hiding (Read)
import Text.Printf (printf)

type S a = Signal a

newtype Addr = Addr {addr :: BitVector 16} deriving (Eq, Num, Enum)
newtype PC   = PC   {pc   :: BitVector 16} deriving (Eq, Num, Enum)
newtype W    = W    {w    :: BitVector 16} deriving (Eq, Num, Enum)
newtype Reg  = Reg  {reg  :: BitVector 4}  deriving (Eq, Num, Enum)

data Write a = Write a W | NoWrite deriving (Eq, Show)

data Read a = Read a | NoRead deriving (Eq, Show)

data Jump = Jump PC | NoJump deriving (Eq, Show)

data Validity = Invalid | Valid deriving (Eq, Show)

newtype Predicted a = Predicted {prediction :: a} deriving (Eq, Num)

instance Show Addr where show (Addr a) = printf "[Addr %x]" (fromEnum a)
instance Show PC where show (PC a)     = printf "[PC %x]"   (fromEnum a)
instance Show W where show (W a)       = printf "[W %x]"    (fromEnum a)
instance Show Reg where show (Reg a)   = printf "[Reg %x]"  (fromEnum a)
instance Show a => Show (Predicted a) where 
    show (Predicted a) = printf "[Pred %s]" (show a)