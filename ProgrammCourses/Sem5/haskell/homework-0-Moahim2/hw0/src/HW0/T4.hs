module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix recursion
  where
    recursion _ []               = []
    recursion newRecursion (h:t) = f h : newRecursion t

fib :: Natural -> Natural
fib = fix recursion (0, 1)
  where
    recursion _ (a, _) 0            = a
    recursion newRecursion (a, b) n = newRecursion (b, a + b) $ n - 1

fac :: Natural -> Natural
fac = fix recursion
  where
    recursion _ 0            = 1
    recursion newRecursion n = n * newRecursion (n - 1)
