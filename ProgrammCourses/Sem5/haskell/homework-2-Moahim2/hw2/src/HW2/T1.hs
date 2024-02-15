module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ curFold Leaf                      = curFold
tfoldr f curFold (Branch _ left val right) = tfoldr f (f val (tfoldr f curFold right)) left
