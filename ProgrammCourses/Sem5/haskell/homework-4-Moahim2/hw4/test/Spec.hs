import Test.Hspec
import Test.QuickCheck

import HW4.T2
import HW4.Types

import Numeric

newtype AExpr a = ARB Expr
  deriving Show

instance  Arbitrary (AExpr a) where
  arbitrary = do
    values <- vectorOf 10 arbitrary
    expr <- genExprRec values
    return $ ARB expr
      where
        genExprRec (x : xs) = do
          i <- arbitrary
          expr <- genExprRec xs
          area <- chooseInt (0, 1)
          let t = [((+), Add), ((-), Sub), ((*), Mul), ((/), Div)] !! (i `mod` 4)
          if area == 0 then return $ Op $ snd (t::(Double -> Double -> Double, Expr -> Expr -> Prim Expr)) (Val x) expr
          else return $ Op $ snd t expr (Val x)
        genExprRec [] = Val <$> arbitrary

exprToStringImpl :: Expr -> Expr -> Char -> String
exprToStringImpl a b op = "(" ++ exprToString a ++ ") " ++ [op] ++ " (" ++ exprToString b ++ ")"

-- formatting testing expr to string
exprToString :: Expr -> String
exprToString (Op (Add a b)) = exprToStringImpl a b '+'
exprToString (Op (Sub a b)) = exprToStringImpl a b '-'
exprToString (Op (Mul a b)) = exprToStringImpl a b '*'
exprToString (Op (Div a b)) = exprToStringImpl a b '/'
exprToString (Val val)      = showFFloat Nothing val ""
exprToString _              = undefined

main :: IO ()
main = hspec $ do
 describe "T2" $ do
   it "Tests from task" $
     parseExpr "3.14 + 1.618 * 2" == HW4.Types.Success (Op (Add (Val 3.14) (Op (Mul (Val 1.618) (Val 2.0)))))
     && parseExpr "2 * (1 + 3)" == HW4.Types.Success (Op (Mul (Val 2.0) (Op (Add (Val 1.0) (Val 3.0)))))
     && parseExpr "24 + Hello" == Error (ErrorAtPos 3)
   it "Simple big test" $
    let arr = replicate 1000000 "+1"
        expr = '1' : concat arr
    in parseExpr expr /= Error (ErrorAtPos 1)
   it "Random tests" $
    property $ \(ARB expr) ->
      parseExpr (exprToString expr) `shouldBe` HW4.Types.Success expr
