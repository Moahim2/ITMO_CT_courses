{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

import HW6.T2

data Proxy a = Proxy
  deriving Show

type ExampleSet= '[ "a", "b" ] :: TSet

test1 :: Proxy 'True
test1 = Proxy @(Contains "a" ExampleSet)

test2 :: Proxy '[ "b" ]
test2 = Proxy @(Delete "a" ExampleSet)

main :: IO ()
main = do
  -- for fix warnings for compile test for T2
  print test1
  print test2
