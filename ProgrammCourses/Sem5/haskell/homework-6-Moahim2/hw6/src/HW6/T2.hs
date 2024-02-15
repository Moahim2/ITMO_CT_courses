{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains a (a : _)  = 'True
  Contains a (_ : xs) = Contains a xs
  Contains _ '[]      = 'False

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete a (a : xs) = xs
  Delete a (x : xs) = x : Delete a xs
  Delete _ '[]      = '[]
  
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add a (a : xs) = Add a xs
  Add a (x : xs) = x : Add a xs
  Add a '[]      = '[a]
