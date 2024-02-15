module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ = id

ns :: Nat a -> Nat a
ns n f = n f . f

nplus :: Nat a -> Nat a -> Nat a
nplus n1 n2 f = n1 f . n2 f

nmult :: Nat a -> Nat a -> Nat a
nmult = (.)

nFromNatural :: Natural -> Nat a
nFromNatural 0   = nz
nFromNatural nat = ns . nFromNatural $ nat - 1

-- ((a -> a) -> a -> a) -> a
nToNum :: Num a => Nat a -> a
nToNum f = f (+1) 0
