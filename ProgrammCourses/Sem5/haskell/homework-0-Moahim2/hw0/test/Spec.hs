import Test.Hspec
import Test.QuickCheck

import HW0.T4
import HW0.T5
import HW0.T6

import GHC.Natural (Natural, intToNatural)

main :: IO ()
main = hspec $ do
--  T1,T2,T3 - test is compile
  describe "HW0.T4" $ do
    it "repeat'" $
      property $ \count n ->
        replicate (count::Int) (n::Int) == take (count::Int) (repeat' (n::Int))
    it "map'" $
      property $ \x ->
        let list = (x :: [Int])
            f = \i -> i * i + 1
            mapList = map f list
            myMapList = map' f list
         in
            mapList == myMapList
    it "fib" $
      fib 0 == 0 &&
      fib 1 == 1 &&
      fib 2 == 1 &&
      fib 5 == 5 &&
      fib 10 == 55 &&
      fib 20 == 6765 &&
      fib 50 == 12586269025
    it "fac" $
      fac 0 == 1 &&
      fac 1 == 1 &&
      fac 2 == 2 &&
      fac 5 == 120 &&
      fac 10 == 3628800
  describe "HW0.T5" $ do
    it "nz" $
      nToNum (nz::Nat Int) == 0
    it "ns" $
      property $ \x ->
        testNatural x (\n -> nToNum ((ns::Nat Int -> Nat Int) . nFromNatural $ n) == 1 + nToNum (nFromNatural n))
    it "nplus" $
      property $ \x y->
        let
          f p q = nToNum ((nplus::Nat Natural-> Nat Natural -> Nat Natural) (nFromNatural p) $ nFromNatural q) == p + q
        in
          testNatural2 (x::Int) (y::Int) f
    it "nmult" $
      property $ \x y->
        let
          f p q = nToNum ((nmult::Nat Natural-> Nat Natural -> Nat Natural) (nFromNatural p) $ nFromNatural q) == p * q
        in
          testNatural2 (x::Int) (y::Int) f
  describe "HW0.T6" $ do --not test but example of working whnf
    it "a" $
      case a_whnf of
        (Left str1, Left str2) -> str1 == str2
        _                      -> False
    it "b" $
      case b_whnf of
        (False:_) -> True
        _         -> False
    it "c" $
      case c_whnf of
        "Y" -> True
        _   -> False


testNatural:: Int -> (Natural -> Bool) -> Bool
testNatural x f = x <= 0 || f (intToNatural x)

testNatural2:: Int -> Int -> (Natural -> Natural -> Bool) -> Bool
testNatural2 x y f = x <= 0 || y <= 0 || f (intToNatural x) (intToNatural y)
