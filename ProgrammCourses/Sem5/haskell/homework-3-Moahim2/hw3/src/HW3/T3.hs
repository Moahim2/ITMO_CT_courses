module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption (Some (Some val)) = Some val
joinOption _                 = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success a) = a
joinExcept (Error e)   = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((val :# e1) :# e2) = val :# e2 <> e1

joinList :: List (List a) -> List a
joinList ((a :. leftListTail) :. listsTail) = a :. joinList (leftListTail :. listsTail)
joinList (Nil :. listsTail)                 = joinList listsTail
joinList Nil                                = Nil

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F externalF) = F $ \i ->
  let (F innerF) = externalF i
  in innerF i
