-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  , lLeft
  , lRight
  , lGenerator
  , lzToList
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (f <$> ls) (f x) $ f <$> rs

-- |Moving one step to right.
lLeft :: ListZipper a -> ListZipper a
lLeft (LZ (l : ls) c rs) = LZ ls l $ c : rs
lLeft lz                 = lz

-- |Moving one step to left.
lRight :: ListZipper a -> ListZipper a
lRight (LZ ls c (r : rs)) = LZ (c : ls) r rs
lRight lz = lz

lGenerator :: (a -> a)     -- ^ The left generator
           -> (a -> a)     -- ^ The right generator
           -> a            -- ^ The focus value
           -> ListZipper a -- ^ Result.
lGenerator f g x = LZ (iterateTail f x) x $ iterateTail g x
  where
    iterateTail fun = tail . iterate fun


instance Comonad ListZipper where
  extract (LZ _ x _) = x

  duplicate = lGenerator lLeft lRight

  --extend f = fmap f . duplicate - minimal pragma id duplicate in our case.

-- |Make list with summary size @n@.
lzToList :: Int          -- ^ n
         -> ListZipper a -- ^ list
         -> [a]          -- ^ result.
lzToList n (LZ ls x rs ) = reverse (take s1 ls) ++ x : take s2 rs
  where
    t = n `div` 2
    (s1, s2) = if n `mod` 2 == 1
          then (t, t)
          else (t, t - 1)
