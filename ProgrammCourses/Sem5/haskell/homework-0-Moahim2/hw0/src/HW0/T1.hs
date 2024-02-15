{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso lToR rToL
  where
    lToR (a, (b, c)) = ((a, b), c)
    rToL ((a, b), c) = (a, (b, c))

--Iso (runIso assocPair) (runIso (flipIso assocPair))- compile too)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso lToR rToL
  where
    lToR (Left a)          = Left (Left a)
    lToR (Right (Left b))  = Left (Right b)
    lToR (Right (Right c)) = Right c

    rToL (Left (Left a))   = Left a
    rToL (Left (Right b))  = Right (Left b)
    rToL (Right c)         = Right (Right c)
