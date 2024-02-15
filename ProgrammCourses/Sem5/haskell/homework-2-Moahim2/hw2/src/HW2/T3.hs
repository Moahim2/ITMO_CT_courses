module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable (fold)

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap eToP
  where
    eToP (Left a)  = (a, mempty)
    eToP (Right b) = (mempty, b)
