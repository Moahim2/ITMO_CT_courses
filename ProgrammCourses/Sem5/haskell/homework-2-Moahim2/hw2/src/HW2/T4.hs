module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last a) rightList        = a :+ rightList
  (<>) (a :+ leftTail) rightList = a :+ (leftTail <> rightList)

data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This i <> This j         = This (i <> j)
  That i <> That j         = That (i <> j)
  Both ai bi <> Both aj bj = Both (ai <> aj) (bi <> bj)
  This i <> That j         = Both i j
  That i <> This j         = Both j i
  Both ai bi <> That bj    = Both ai (bi <> bj)
  Both ai bi <> This aj    = Both (ai <> aj) bi
  That bi <> Both aj bj    = Both aj (bi <> bj)
  This ai <> Both aj bj    = Both (ai <> aj) bj


newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  leftDS <> DS []     = leftDS
  DS [] <> rightDS    = rightDS
  DS left <> DS right = DS $ left ++ ('.' : right)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
    F f1 <> F f2 = F $ f1 . f2

instance Monoid (Fun a) where
  mempty = F id
