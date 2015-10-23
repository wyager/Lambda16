{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Safety.Stages (Stage(..), Succ, Pred, IsStage, Rep, rep) where

import CLaSH.Prelude

data Stage = F | D | R | X

type family Succ (s :: Stage) :: Stage where
    Succ F = D
    Succ D = R
    Succ R = X

type family Pred (s :: Stage) :: Stage where
    Pred X = R
    Pred R = D
    Pred D = F

class IsStage (stage :: Stage) where
    rep :: Rep stage

data Rep (a :: Stage) = Rep String

instance Show (Rep a) where
    show (Rep s) = s

instance IsStage F where rep = Rep "F"
instance IsStage D where rep = Rep "D"
instance IsStage R where rep = Rep "R"
instance IsStage X where rep = Rep "X"
