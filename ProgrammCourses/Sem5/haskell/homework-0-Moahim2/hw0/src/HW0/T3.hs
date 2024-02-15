module HW0.T3
  ( compose
  , contract
  , i
  , k
  , permute
  , s
  ) where

-- S
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

-- K
k :: a -> b -> a
k x _ = x

-- I
i :: a -> a
i = s k k

-- all is obtained by the algorithm of lambda function expression through the SKI basis.
--I replaced 'i' with sk, because I parsed the task condition like this.
-- B
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (s (k s) (s (k k) (s (k s) (s (k k) (s k k))))) (k (s (s (k s) (s (k k) (s k k))) (k (s k k))))

-- W
contract :: (a -> a -> b) -> (a -> b)
contract = s (s (k s) (s (s (k s) (s (k k) (s k k))) (k (s k k)))) (k (s k k))

-- C
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k s) (s (k k) (s (k s) (s (s (k s) (s (k k) (s k k))) (k (s k k)))))) (k (s (k k) (s k k)))
