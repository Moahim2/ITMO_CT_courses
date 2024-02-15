-- | This module contains the types from hw3 that are also
-- needed for hw4.
module HW4.Types
  ( Annotated (..)
  , Except (..)
  , Expr (..)
  , Prim (..)
  , State (..)
  , mapExcept
  , mapAnnotated
  , wrapExcept
  ) where

data Except e a = Error e | Success a
  deriving (Show, Eq)

data Annotated e a = a :# e
  deriving (Show, Eq)

newtype State s a = S {runS :: s -> Annotated s a}

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show, Eq)

data Expr = Val Double | Op (Prim Expr)
  deriving (Show, Eq)

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)


mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Success val) = Success $ f val
mapExcept _ (Error t)     = Error t

wrapExcept :: a -> Except e a
wrapExcept = Success

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (val :# ann) = f val :# ann
