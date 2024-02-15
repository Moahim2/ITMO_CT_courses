module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    nFromNatural,
    nToNum,
    ncmp,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Data.Maybe
import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus a Z     = a
nplus a (S c) = S $ a `nplus` c

nmult :: N -> N -> N
nmult _ Z     = Z
nmult a (S c) = (a `nmult` c) `nplus` a

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = a `nsub` b

ncmp :: N -> N -> Ordering
ncmp a b = case a `nsub` b of
  Nothing -> LT
  Just Z  -> EQ
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + nToNum n

nEven :: N -> Bool
nEven Z     = True
nEven (S n) = not $ nEven n

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv a b = case a `nsub` b of
  Nothing  -> Z
  Just res -> S Z `nplus` (res `ndiv` b)

nmod :: N -> N -> N
nmod a b =
  let tmp = a `ndiv` b
   in case tmp of
        Z -> a
        _ -> fromJust $ a `nsub` (b `nmult` tmp)
