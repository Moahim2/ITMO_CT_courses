{-# LANGUAGE DerivingVia #-}

import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..))
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)

import Test.Hspec
import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.Sequence as S
import qualified Data.Text as T

import Control.Monad.Identity
import Data.Map (fromList)
import Data.Ratio ((%))
import Data.Word (Word8)

newtype MyIdentity a = HII {runMyIdentity :: a} deriving (Functor, Applicative, Monad)
                                                via Identity

-- in fact, here it is - 'instance Identity' (from the description of the task to the T7)
instance HiMonad (MyIdentity) where
  runAction _ = HII {runMyIdentity = HiValueNull}


simpleParserTest :: String -> HiExpr -> Bool
simpleParserTest input expectedExpr = case parse input of
  Left _       -> False
  Right hiExpr -> expectedExpr == hiExpr

simpleParserErrorTest :: String -> Bool
simpleParserErrorTest input = case parse input of
  Left _  -> True
  Right _ -> False

simpleParserEquivalenceTest :: String -> String -> Bool
simpleParserEquivalenceTest input1 input2 = case parse input1 of
  Left _       -> False
  Right hiExpr -> simpleParserTest input2 hiExpr


simpleOperatorParserTest :: String -> HiFun -> Bool
simpleOperatorParserTest op opConstr = simpleParserTest ("1 " ++ op ++ " 1")
           (HiExprApply (HiExprValue $ HiValueFunction opConstr)
             [
               HiExprValue $ hiValueInt 1,
               HiExprValue $ hiValueInt 1
             ])

eval' :: HiExpr -> Either HiError HiValue
eval' = runMyIdentity . eval

simpleEvaluatorErrorTest :: HiExpr -> HiError -> Bool
simpleEvaluatorErrorTest input expectedError = case eval' input of
  Left hiError -> expectedError == hiError
  _            -> False

simpleEvaluatorValueTest :: HiExpr -> HiValue -> Bool
simpleEvaluatorValueTest input expectedValue = case eval' input of
  Right hiValue -> expectedValue == hiValue
  _             -> False

simpleEvaluatorErrorInputTest :: String -> HiError -> Bool
simpleEvaluatorErrorInputTest inputStr expectedError = case parse inputStr of
  Left _     -> False
  Right expr -> simpleEvaluatorErrorTest expr expectedError

simpleEvaluatorValueInputTest :: String -> HiValue -> Bool
simpleEvaluatorValueInputTest inputStr expectedValue = case parse inputStr of
  Left _     -> False
  Right expr -> simpleEvaluatorValueTest expr expectedValue

simplePrettyTest :: HiValue -> String -> Bool
simplePrettyTest hiValue expectedStr = expectedStr == show (prettyValue hiValue)

simpleFullLangTest :: String -> String -> Bool
simpleFullLangTest input expected = case parse input of
  Left _ -> False
  Right expr -> case eval' expr of
    Left _      -> False
    Right value -> simplePrettyTest value expected

simpleFullLangEvalErrorTest :: String -> HiError -> Bool
simpleFullLangEvalErrorTest input expectedEvalError = case parse input of
  Left _ -> False
  Right expr -> case eval' expr of
    Left evalError -> expectedEvalError == evalError
    Right _        -> False

hiExprFun :: HiFun -> HiExpr
hiExprFun = HiExprValue . HiValueFunction

hiValueInt :: Int -> HiValue
hiValueInt i = HiValueNumber $ toRational i

hiValueString :: String -> HiValue
hiValueString = HiValueString . T.pack

hiValueBytes :: [Word8] -> HiValue
hiValueBytes = HiValueBytes . BS.pack

hiValueAction :: HiAction -> HiValue
hiValueAction action = HiValueAction action

hiExprAction :: HiAction -> HiExpr
hiExprAction = HiExprValue . hiValueAction

mapToExprs :: [(HiValue, HiValue)] -> [(HiExpr, HiExpr)]
mapToExprs arr = map (\(k, a) -> (HiExprValue k, HiExprValue a)) arr


hiValueDict :: [(HiValue, HiValue)] -> HiValue
hiValueDict = HiValueDict . fromList

main :: IO ()
main = hspec $ do
 describe "T1" $ do
   describe "Parser" $ do
      it "1" $ simpleParserTest "div" (hiExprFun HiFunDiv)
      it "2" $ simpleParserTest "mul" (hiExprFun HiFunMul)
      it "3" $ simpleParserTest "add" (hiExprFun HiFunAdd)
      it "4" $ simpleParserTest "sub" (hiExprFun HiFunSub)

      it "5" $ simpleParserTest "2" (HiExprValue (hiValueInt 2))
      it "6" $ simpleParserTest "3.14" (HiExprValue (HiValueNumber $ 314 % 100))
      it "7" $ simpleParserTest "-1.618" (HiExprValue (HiValueNumber $ -1618 % 1000))
      it "8" $ simpleParserTest "1.2e5" (HiExprValue (HiValueNumber $ 120000 % 1))

      it "9" $ simpleParserTest "div(add(10, 15.1), 3)" (HiExprApply (hiExprFun HiFunDiv)
                                                  [
                                                    HiExprApply (hiExprFun HiFunAdd)
                                                      [
                                                        HiExprValue (HiValueNumber (10 % 1)),
                                                        HiExprValue (HiValueNumber (151 % 10))
                                                      ],
                                                    HiExprValue (HiValueNumber (3 % 1))
                                                  ])

   describe "Evaluator" $ do
      it "1" $ simpleEvaluatorValueInputTest "add(500, 12)" (hiValueInt 512)
      it "2" $ simpleEvaluatorValueInputTest "sub(10, 100)" (hiValueInt (-90))
      it "3" $ simpleEvaluatorValueInputTest "mul(23, 768)" (hiValueInt 17664)
      it "4" $ simpleEvaluatorValueInputTest "div(57, 190)" (HiValueNumber $ 3 % 10)

      it "5" $ simpleEvaluatorValueInputTest "div(add(mul(2, 5), 1), sub(11,6))" (HiValueNumber $ 22 % 10)

      it "6" $ simpleEvaluatorErrorInputTest "sub(1)" HiErrorArityMismatch
      it "7" $ simpleEvaluatorErrorInputTest "sub(1, 2, 3)" HiErrorArityMismatch
      it "8" $ simpleEvaluatorErrorInputTest "div(1, 0)" HiErrorDivideByZero
      it "9" $ simpleEvaluatorErrorInputTest "div(1, sub(5, 5))" HiErrorDivideByZero
      it "10" $ simpleEvaluatorErrorInputTest "15(2)" HiErrorInvalidFunction
      it "11" $ simpleEvaluatorErrorInputTest "sub(10, add)" HiErrorInvalidArgument

      it "12 faq" $ simpleEvaluatorErrorInputTest "add(1, 2, 3 / 0)" HiErrorArityMismatch

   describe "Pretty-Value" $ do
      it "1" $ simplePrettyTest (HiValueFunction HiFunDiv) "div"
      it "2" $ simplePrettyTest (HiValueFunction HiFunMul) "mul"
      it "3" $ simplePrettyTest (HiValueFunction HiFunAdd) "add"
      it "4" $ simplePrettyTest (HiValueFunction HiFunSub) "sub"

      it "5" $ simplePrettyTest (hiValueInt 42) "42"
      it "6" $ simplePrettyTest (hiValueInt (-8)) "-8"
      it "7" $ simplePrettyTest (hiValueInt 15) "15"

      it "8" $ simplePrettyTest (HiValueNumber $ 314 % 100) "3.14"
      it "9" $ simplePrettyTest (HiValueNumber $ -815 % 100) "-8.15"
      it "10" $ simplePrettyTest (HiValueNumber $ 7701 % 100) "77.01"

      it "11" $ simplePrettyTest (HiValueNumber $ 1 % 3) "1/3"
      it "12" $ simplePrettyTest (HiValueNumber $ -1 % 7) "-1/7"
      it "13" $ simplePrettyTest (HiValueNumber $ 3 % 11) "3/11"

      it "14" $ simplePrettyTest (HiValueNumber $ 16 % 3) "5 + 1/3"
      it "15" $ simplePrettyTest (HiValueNumber $ -71 % 7) "-10 - 1/7"
      it "16" $ simplePrettyTest (HiValueNumber $ 267 % 11) "24 + 3/11"

   describe "Full-Lang-Test" $ do
      it "1" $ simpleFullLangTest "100" "100"
      it "2" $ simpleFullLangTest "-15" "-15"
      it "3" $ simpleFullLangTest "add(100, -15)" "85"
      it "4" $ simpleFullLangTest "add(3, div(14, 100))" "3.14"
      it "5" $ simpleFullLangTest "div(10, 3)" "3 + 1/3"
      it "6" $ simpleFullLangTest "sub(mul(201, 11), 0.33)" "2210.67"

 describe "T2" $ do
   describe "Parser" $ do
      it "1" $ simpleParserTest "true" (HiExprValue $ HiValueBool True)
      it "2" $ simpleParserTest "false" (HiExprValue $ HiValueBool False)

      it "3" $ simpleParserTest "not" (hiExprFun HiFunNot)
      it "4" $ simpleParserTest "and" (hiExprFun HiFunAnd)
      it "5" $ simpleParserTest "or" (hiExprFun HiFunOr)
      it "6" $ simpleParserTest "less-than" (hiExprFun HiFunLessThan)
      it "7" $ simpleParserTest "greater-than" (hiExprFun HiFunGreaterThan)
      it "8" $ simpleParserTest "equals" (hiExprFun HiFunEquals)
      it "9" $ simpleParserTest "not-less-than" (hiExprFun HiFunNotLessThan)
      it "10" $ simpleParserTest "not-greater-than" (hiExprFun HiFunNotGreaterThan)
      it "11" $ simpleParserTest "not-equals" (hiExprFun HiFunNotEquals)
      it "12" $ simpleParserTest "if" (hiExprFun HiFunIf)

   describe "Evaluator" $ do
      it "1" $ simpleEvaluatorValueInputTest "not(true)" (HiValueBool False)
      it "2" $ simpleEvaluatorValueInputTest "and(true, false)" (HiValueBool False)
      it "3" $ simpleEvaluatorValueInputTest "or(true, false)" (HiValueBool True)

      it "4" $ simpleEvaluatorValueInputTest "equals(10, 10)" (HiValueBool True)
      it "5" $ simpleEvaluatorValueInputTest "equals(false, false)" (HiValueBool True)
      it "6" $ simpleEvaluatorValueInputTest "equals(3, 10)" (HiValueBool False)
      it "7" $ simpleEvaluatorValueInputTest "equals(1, true)" (HiValueBool False)

      it "8" $ simpleEvaluatorValueInputTest "less-than(3, 10)" (HiValueBool True)
      it "9" $ simpleEvaluatorValueInputTest "less-than(false, true)" (HiValueBool True)
      it "10" $ simpleEvaluatorValueInputTest "less-than(false, 0)" (HiValueBool True)

      it "11" $ simpleEvaluatorValueInputTest "if(false, false, true)" (HiValueBool True)
      it "12" $ simpleEvaluatorValueInputTest "if(true, false, true)" (HiValueBool False)

   describe "Full-Lang-Test" $ do
      it "1" $ simpleFullLangTest "false" "false"
      it "2" $ simpleFullLangTest "equals(add(2, 2), 4)" "true"
      it "3" $ simpleFullLangTest "less-than(mul(999, 99), 10000)" "false"
      it "4" $ simpleFullLangTest "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" "-1"
      it "5" $ simpleFullLangTest "and(less-than(0, 1), less-than(1, 0))" "false"

      it "6" $ simpleFullLangTest "if(true, add, mul)" "add"
      it "7" $ simpleFullLangTest "if(true, add, mul)(10, 10)" "20"
      it "8" $ simpleFullLangTest "if(false, add, mul)(10, 10)" "100"

      it "9" $ simpleFullLangTest "equals(add, add)" "true"
      it "10" $ simpleFullLangTest "equals(add, mul)" "false"

 describe "T3" $ do
    describe "Parser" $ do
      it "1" $ simpleOperatorParserTest "/" HiFunDiv
      it "2" $ simpleOperatorParserTest "*" HiFunMul
      it "3" $ simpleOperatorParserTest "+" HiFunAdd
      it "4" $ simpleOperatorParserTest "-" HiFunSub
      it "5" $ simpleOperatorParserTest "<" HiFunLessThan
      it "6" $ simpleOperatorParserTest ">" HiFunGreaterThan
      it "7" $ simpleOperatorParserTest ">=" HiFunNotLessThan
      it "8" $ simpleOperatorParserTest "<=" HiFunNotGreaterThan
      it "9" $ simpleOperatorParserTest "==" HiFunEquals
      it "10" $ simpleOperatorParserTest "/=" HiFunNotEquals
      it "11" $ simpleOperatorParserTest "&&" HiFunAnd
      it "12" $ simpleOperatorParserTest "||" HiFunOr

    describe "Full-Lang-Test" $ do
      it "1" $ simpleFullLangTest "2 + 2" "4"
      it "2" $ simpleFullLangTest "2 + 2 * 3" "8"
      it "3" $ simpleFullLangTest "(2 + 2) * 3" "12"
      it "4" $ simpleFullLangTest "2 + 2 * 3 == (2 + 2) * 3" "false"
      it "5" $ simpleFullLangTest "10 == 2*5 && 143 == 11*13" "true"
      it "6 (faq)" $ simpleFullLangEvalErrorTest "(div(1))(10)" HiErrorArityMismatch


 describe "T4" $ do
   describe "Parser" $ do
      it "1" $ simpleParserTest "length" (hiExprFun HiFunLength)
      it "2" $ simpleParserTest "to-upper" (hiExprFun HiFunToUpper)
      it "3" $ simpleParserTest "to-lower" (hiExprFun HiFunToLower)
      it "4" $ simpleParserTest "reverse" (hiExprFun HiFunReverse)
      it "5" $ simpleParserTest "trim" (hiExprFun HiFunTrim)

      it "6" $ simpleParserTest "\"42\"" (HiExprValue $ hiValueString "42")
      it "7" $ simpleParserTest "\"hello\"" (HiExprValue $ hiValueString "hello")
      it "8" $ simpleParserTest "\"header\nfooter\"" (HiExprValue $ hiValueString "header\nfooter")

      it "9" $ simpleParserTest "null" (HiExprValue HiValueNull)

   describe "Evaluator" $ do
      context "String specific operations" $ do
        it "1" $ simpleEvaluatorValueInputTest "length(\"Hello World\")" (hiValueInt 11)
        it "2" $ simpleEvaluatorValueInputTest "to-upper(\"Hello World\")" (hiValueString "HELLO WORLD")
        it "3" $ simpleEvaluatorValueInputTest "to-lower(\"Hello World\")" (hiValueString "hello world")
        it "4" $ simpleEvaluatorValueInputTest "reverse(\"stressed\")" (hiValueString "desserts")
        it "5" $ simpleEvaluatorValueInputTest "trim(\"   Hello World     \")" (hiValueString "Hello World")
      context "String arithmetic operations" $ do
        it "1" $ simpleEvaluatorValueInputTest "\"Hello\" + \"World\"" (hiValueString "HelloWorld")
        it "2" $ simpleEvaluatorValueInputTest "\"Cat\" * 5" (hiValueString "CatCatCatCatCat")
        it "3" $ simpleEvaluatorValueInputTest "\"/home/user\" / \"hi\"" (hiValueString "/home/user/hi")
      context "Slices" $ do
        it "1" $ simpleEvaluatorValueInputTest "\"Hello World\"(0)" (hiValueString "H")
        it "2" $ simpleEvaluatorValueInputTest "\"Hello World\"(7)" (hiValueString "o")

        it "3" $ simpleEvaluatorValueInputTest "\"Hello World\"(-1)" HiValueNull
        it "4" $ simpleEvaluatorValueInputTest "\"Hello World\"(99)" HiValueNull

        it "5" $ simpleEvaluatorValueInputTest "\"Hello World\"(0, 5)" (hiValueString "Hello")
        it "6" $ simpleEvaluatorValueInputTest "\"Hello World\"(2, 4)" (hiValueString "ll")
        it "7 (faq)" $ simpleEvaluatorValueInputTest "\"abc\"(0, 5)" (hiValueString "abc")
        it "8 (faq)" $ simpleEvaluatorValueInputTest "\"abc\"(3, 1)" (hiValueString "")
      context "Slices adv" $ do
        it "1" $ simpleEvaluatorValueInputTest "\"Hello World\"(0, -4)" (hiValueString "Hello W")
        it "2" $ simpleEvaluatorValueInputTest "\"Hello World\"(-4, -1)" (hiValueString "orl")

        it "3" $ simpleEvaluatorValueInputTest "\"Hello, World\"(2, null)" (hiValueString "llo, World")
        it "4" $ simpleEvaluatorValueInputTest "\"Hello, World\"(null, 5)" (hiValueString "Hello")
        it "5 (faq)" $ simpleEvaluatorValueInputTest "\"abcd\"(3, -3)" (hiValueString "")
        it "6 my faq" $  simpleEvaluatorValueInputTest "\"abc\"(10, 10)" (hiValueString "")

   describe "Full-Lang-Test" $ do
      context "Common" $ do
        it "2" $ simpleFullLangTest "\"Hello\" == \"World\"" "false"
        it "3" $ simpleFullLangTest "length(\"Hello\" + \"World\")" "10"
        it "4" $ simpleFullLangTest "length(\"hehe\" * 5) / 3" "6 + 2/3"

      context "Errors (mandatory)" $ do
        it "1 (faq)" $ simpleFullLangEvalErrorTest "\"abc\"(1, 2.2)" HiErrorInvalidArgument
        it "2" $ simpleFullLangEvalErrorTest "\"abc\"(2.2)" HiErrorInvalidArgument
      context "Errors my optional" $ do
        it "1 (faq)" $ simpleFullLangEvalErrorTest "\"Cat\" * 5.2" HiErrorInvalidArgument


 describe "T5" $ do
   describe "Parser" $ do
     context "Functions" $ do
        it "1" $ simpleParserTest "list" (hiExprFun HiFunList)
        it "2" $ simpleParserTest "range" (hiExprFun HiFunRange)
        it "3" $ simpleParserTest "fold" (hiExprFun HiFunFold)
     context "List constructor[]" $ do
        it "1" $ simpleParserTest "[]" (HiExprApply (HiExprValue (HiValueFunction HiFunList)) [])
        it "2" $ simpleParserTest "[1, 2]" (HiExprApply (HiExprValue (HiValueFunction HiFunList))
                                                        [HiExprValue $ hiValueInt 1, HiExprValue $ hiValueInt 2])
        it "3 my" $ simpleParserTest "[1, \"x\"]" (HiExprApply (HiExprValue (HiValueFunction HiFunList))
                                                  [HiExprValue $ hiValueInt 1, HiExprValue $ hiValueString "x"])
        it "4 recursive [[]]" $  simpleParserTest "[[]]" (HiExprApply (HiExprValue (HiValueFunction HiFunList))
                                                          [HiExprApply (HiExprValue (HiValueFunction HiFunList))[]])
        it "5 my ws test" $ simpleParserEquivalenceTest "[1, [[2, 3]]]" "[1, [  [2,      3]   ]     ]"

        -- not testing in tests
        it "6 my ws test" $ simpleParserEquivalenceTest "[            ]" "[]"
   describe "Evaluator" $ do
     context "List specific operations" $ do
        it "1" $ simpleEvaluatorValueInputTest "list(1, 2, 3)" (HiValueList (S.fromList
                                                               [hiValueInt 1,
                                                               hiValueInt 2,
                                                               hiValueInt 3]))
        it "2" $ simpleEvaluatorValueInputTest "range(5, 10.3)" (HiValueList (S.fromList
                                                                     [hiValueInt 5,
                                                                      hiValueInt 6,
                                                                      hiValueInt 7,
                                                                      hiValueInt 8,
                                                                      hiValueInt 9,
                                                                      hiValueInt 10]))
        it "3" $ simpleEvaluatorValueInputTest "fold(add, [11, 22, 33])" (hiValueInt 66)
        it "4" $ simpleEvaluatorValueInputTest "fold(mul, [11, 22, 33])" (hiValueInt 7986)
        it "5" $ simpleEvaluatorValueInputTest "fold(div, [11, 22, 33])" (HiValueNumber $ 1 % 66)
        it "6 fold faq (only binary)"  $ simpleEvaluatorErrorInputTest "fold(not, [true])" HiErrorInvalidArgument
        it "7 fold faq (only binary)"  $ simpleEvaluatorErrorInputTest "fold(if,[true,1,0])" HiErrorInvalidArgument
        it "8 fold testcases one arg"  $ simpleEvaluatorValueInputTest "fold(add, [1])" (hiValueInt 1)
        it "9 fold testcases zero arg" $ simpleEvaluatorValueInputTest "fold(add, [])" HiValueNull

     context "List aretmetic and string functions" $ do
        it "1" $ simpleEvaluatorValueInputTest "length([1, true, \"Hello\"])" (hiValueInt 3)
        it "2" $ simpleEvaluatorValueInputTest "reverse([1, true, \"Hello\"])" (HiValueList (S.fromList
                                                                                             [hiValueString "Hello",
                                                                                              HiValueBool True,
                                                                                              hiValueInt 1]))
        it "3" $ simpleEvaluatorValueInputTest "[1, 2] + [3, 4, 5]" (HiValueList (S.fromList
                                                                                 [hiValueInt 1,
                                                                                  hiValueInt 2,
                                                                                  hiValueInt 3,
                                                                                  hiValueInt 4,
                                                                                  hiValueInt 5]))
        it "4" $ simpleEvaluatorValueInputTest "[0, \"x\"] * 3" (HiValueList (S.fromList
                                                                                         [hiValueInt 0,
                                                                                          hiValueString "x",
                                                                                          hiValueInt 0,
                                                                                          hiValueString "x",
                                                                                          hiValueInt 0,
                                                                                          hiValueString "x"]))
     context "Slices" $ do
        it "1 + faq" $ simpleEvaluatorValueInputTest "[\"hello\", true, \"world\"](1)" (HiValueBool True)
        it "2 faq" $ simpleEvaluatorValueInputTest "[1,2,3](0)" (hiValueInt 1)
        it "3" $ simpleEvaluatorValueInputTest "[\"hello\", true, \"world\"](1, 3)" (HiValueList (S.fromList
                                                                                             [HiValueBool True,
                                                                                              hiValueString "world"]))
   describe "Pretty-Value" $ do
     it "1" $ simplePrettyTest (HiValueList (S.fromList
                                            [hiValueInt 1,
                                             hiValueInt 2,
                                             hiValueInt 3,
                                             hiValueInt 4,
                                             hiValueInt 5])) "[1, 2, 3, 4, 5]"

   describe "Full-Lang-Test" $ do
     context "Common" $ do
         it "1" $ simpleFullLangTest "list(1, 2, 3, 4, 5)" "[1, 2, 3, 4, 5]"
         it "2" $ simpleFullLangTest "fold(add, [2, 5] * 3)" "21"
         it "3" $ simpleFullLangTest "fold(mul, range(1, 10))" "3628800"
         it "4" $ simpleFullLangTest "[0, true, false, \"hello\", \"world\"](2, 4)" "[false, \"hello\"]"
         it "5" $ simpleFullLangTest "reverse(range(0.5, 70/8))" "[8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5]"
         it "6 fold-recursive" $ simpleFullLangTest "fold(add, [fold(add, [1, 2])])" "3"

 describe "T6" $ do
   describe "Parser" $ do
     context "Functions" $ do
         it "1" $ simpleParserTest "pack-bytes" (hiExprFun HiFunPackBytes)
         it "2" $ simpleParserTest "unpack-bytes" (hiExprFun HiFunUnpackBytes)
         it "3" $ simpleParserTest "zip" (hiExprFun HiFunZip)
         it "4" $ simpleParserTest "unzip" (hiExprFun HiFunUnzip)
         it "5" $ simpleParserTest "encode-utf8" (hiExprFun HiFunEncodeUtf8)
         it "6" $ simpleParserTest "decode-utf8" (hiExprFun HiFunDecodeUtf8)
         it "7" $ simpleParserTest "serialise" (hiExprFun HiFunSerialise)
         it "8" $ simpleParserTest "deserialise" (hiExprFun HiFunDeserialise)
     context "Bytes constructor[# #]" $ do
         it "1 [##]" $ simpleParserTest "[##]" (HiExprValue . hiValueBytes $ [])
         it "2 [#   #]" $ simpleParserTest "[#      #]" (HiExprValue . hiValueBytes $ [])
         it "3" $ simpleParserTest "[# 01 3f ec #]" (HiExprValue . hiValueBytes $ [0x01, 0x3f, 0xec])

         --apparently, this is not tested in github tests ()
         it "4 faq and my" $ simpleParserErrorTest "[# 0 #]"
         it "5 faq and my" $ simpleParserErrorTest "[#01 02 ff aa f#]"
         it "6 faq and my" $ simpleParserErrorTest "[#123 ff#]"
         it "7 faq and my" $ simpleParserErrorTest "[#aaaaaaaaaaaaaaa#]"
         --ws
         it "8 my ws test" $ simpleParserEquivalenceTest "[    [# 01      ff     #]   , add   ]" "[[#01 ff#],add]"
   describe "Evaluator" $ do
     context "Simple bytes specific operations" $ do
         it "1" $ simpleEvaluatorValueInputTest "pack-bytes([ 3, 255, 158, 32 ])" (hiValueBytes [0x03, 0xff, 0x9e, 0x20])
         it "2" $ simpleEvaluatorValueInputTest "unpack-bytes([# 10 20 30 #])" (HiValueList (S.fromList
                                                                                            [hiValueInt 16,
                                                                                             hiValueInt 32,
                                                                                             hiValueInt 48]))
         it "3" $ simpleEvaluatorValueInputTest "encode-utf8(\"Hello!\")"
                                                (hiValueBytes [0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x21])
         it "4" $ simpleEvaluatorValueInputTest "decode-utf8([# 48 65 6c 6c 6f #])" (hiValueString "Hello")
         it "5" $ simpleEvaluatorValueInputTest "decode-utf8([# c3 28 #])" HiValueNull
         it "6 my" $ simpleEvaluatorValueInputTest "zip([#01 ff 01 ff#])" (hiValueBytes [0x78, 0xda, 0x63, 0xfc, 0xcf, 0xf8,
                                                                                        0x1f, 0x00, 0x04, 0x06, 0x02, 0x01])
         it "7 my" $ simpleEvaluatorValueInputTest "unzip([#78 da 63 fc cf f8 1f 00 04 06 02 01#])"
                                                   (hiValueBytes [0x01, 0xff, 0x01, 0xff])
         it "8 my" $ simpleEvaluatorValueInputTest "serialise(add)" (hiValueBytes [0x82, 0x03, 0x81, 0x02])
         it "9 my" $ simpleEvaluatorValueInputTest "deserialise([#82 03 81 02#])" (HiValueFunction HiFunAdd)

         it "10 faq error" $ simpleEvaluatorErrorInputTest "pack-bytes([ 3, 256, 158, 32 ])" HiErrorInvalidArgument
         it "11 faq error" $ simpleEvaluatorErrorInputTest "pack-bytes([ 3, 255, -2, 32 ])" HiErrorInvalidArgument
         it "12 faq error" $ simpleEvaluatorErrorInputTest "serialise(1/0)" HiErrorDivideByZero
         -- zip unzip errors not defined by me
         it "13 my (deserialise -> null)" $ simpleEvaluatorValueInputTest "deserialise([# 00 ff #])" HiValueNull
     context "List aretmetic and string functions" $ do
         it "1" $ simpleEvaluatorValueInputTest "[# 00 ff #] + [# 01 e3 #]" (hiValueBytes [0x00, 0xff, 0x01, 0xe3])
         it "2" $ simpleEvaluatorValueInputTest "[# 00 ff #] * 3" (hiValueBytes [0x00, 0xff, 0x00, 0xff, 0x00, 0xff])
         it "3 length faq" $ simpleEvaluatorValueInputTest "length([# 00 ff #])" (hiValueInt 2)
         it "4 reverse faq" $ simpleEvaluatorValueInputTest "reverse([# 00 ff #])" (hiValueBytes [0xff, 0x00])
     context "Slices" $ do
         it "1" $ simpleEvaluatorValueInputTest "[# 00 ff 01 e3 #](1)" (hiValueInt 255)
         it "2" $ simpleEvaluatorValueInputTest "[# 00 ff 01 e3 #](1,3)" (hiValueBytes [0xff, 0x01])
   describe "Pretty-Value" $ do
         it "1" $ simplePrettyTest (hiValueBytes [0x00, 0xff, 0x00, 0xff, 0x00, 0xff]) "[#00 ff 00 ff 00 ff#]"

   describe "Full-Lang-Test" $ do
         it "1" $ simpleFullLangTest "pack-bytes(range(30, 40))" "[#1e 1f 20 21 22 23 24 25 26 27 28#]"
         it "2" $ simpleFullLangTest "zip(encode-utf8(\"Hello, World!\" * 1000))" "[#78 da ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28#]"
         it "3" $ simpleFullLangTest "decode-utf8([# 68 69 #] * 5)" "\"hihihihihi\""
         it "4" $ simpleFullLangTest "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" "[#01 02 03#]"

   describe "Random test for lows" $ do
         it "zip-unzip" $
          property $ \str ->
            let strI = filter (\i -> i /= '\\' && i /= '\"') str in
              simpleFullLangTest ("decode-utf8(unzip(zip(encode-utf8(\"" ++ strI ++ "\"))))") ("\"" ++ strI ++ "\"")
         it "serialise-deserialise" $
          property $ \str ->
            let strI = filter (\i -> i /= '\\' && i /= '\"') str in
              simpleFullLangTest ("deserialise(serialise(\"" ++ strI ++ "\"))") ("\"" ++ strI ++ "\"")


 describe "T7" $ do
   describe "Parser" $ do
        it "1" $ simpleParserTest "read" (hiExprFun HiFunRead)
        it "2" $ simpleParserTest "write" (hiExprFun HiFunWrite)
        it "3" $ simpleParserTest "mkdir" (hiExprFun HiFunMkDir)
        it "4" $ simpleParserTest "cd" (hiExprFun HiFunChDir)

        it "5" $ simpleParserTest "cwd" (hiExprAction HiActionCwd)

        it "6" $ simpleParserTest "cwd!" (HiExprRun $ hiExprAction HiActionCwd)
        it "7" $ simpleParserTest "read(\"hello.txt\")!"
                                  (HiExprRun $ HiExprApply (hiExprFun HiFunRead) [HiExprValue $ hiValueString "hello.txt"])
        it "8" $ simpleParserTest "mkdir(\"projects\")!"
                                  (HiExprRun $ HiExprApply (hiExprFun HiFunMkDir) [HiExprValue $ hiValueString "projects"])

        -- I love faq (now! - now!) and i hate whitespaces
        it "9 I FIX THIS F...ING parsing bug!!!" $ simpleParserTest "1! - 1!" (HiExprApply (hiExprFun HiFunSub)
                                                                              [HiExprRun . HiExprValue $ hiValueInt 1,
                                                                               HiExprRun . HiExprValue $ hiValueInt 1])
   describe "Evaluator" $ do
        it "1" $ simpleEvaluatorValueInputTest "read(\"hi.txt\")" (hiValueAction $ HiActionRead "hi.txt")
        it "2" $ simpleEvaluatorValueInputTest "write(\"hi.txt\", \"Hi!\")"
                                               (hiValueAction $ HiActionWrite "hi.txt" (BS.pack [0x48, 0x69, 0x21 ]))
        it "3" $ simpleEvaluatorValueInputTest "mkdir(\"dir\")" (hiValueAction $ HiActionMkDir "dir")
        it "4" $ simpleEvaluatorValueInputTest "cd(\"dir\")" (hiValueAction $ HiActionChDir "dir")

        it "5 faq (!!)" $ simpleEvaluatorErrorInputTest "read(\"hi.txt\")!!" HiErrorInvalidArgument
        it "6 faq (!!)" $ simpleEvaluatorErrorInputTest "cwd!!" HiErrorInvalidArgument
        it "7 faq (!!)" $ simpleEvaluatorErrorInputTest "(cwd!)!" HiErrorInvalidArgument


   describe "Full-Lang-Test" $ do
        it "1" $ simpleFullLangTest "read" "read"
        it "2" $ simpleFullLangTest "read(\"hi.txt\")" "read(\"hi.txt\")"
        it "3 my" $ simpleFullLangTest "write(\"hi.txt\", \"Hi!\")" "write(\"hi.txt\", [#48 69 21#])"
        it "4 my" $ simpleFullLangTest "mkdir(\"hi.txt\")" "mkdir(\"hi.txt\")"
        it "5 my" $ simpleFullLangTest "cd(\"hi.txt\")" "cd(\"hi.txt\")"
        it "6 my" $ simpleFullLangTest "cwd" "cwd"

 describe "T8" $ do
   describe "Parser" $ do
        it "1" $ simpleParserTest "parse-time" (hiExprFun HiFunParseTime)
        it "2" $ simpleParserTest "now" (hiExprAction HiActionNow)
   describe "Evaluator" $ do
        it "1" $ simpleEvaluatorValueInputTest "parse-time(\"2021-12-15 00:42:33.02949461 UTC\")"
                                              (HiValueTime (read "2021-12-15 00:42:33.02949461 UTC"))
        it "2" $ simpleEvaluatorValueInputTest "parse-time(\"abracadabra\")" HiValueNull

        it "3" $ simpleEvaluatorValueInputTest "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000"
                                               (HiValueTime (read "2021-12-15 00:16:40 UTC"))
        it "4" $ simpleEvaluatorValueInputTest "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")"
                                               (HiValueNumber $ 3351843755 % 1000000000)

   describe "Full-Lang-Test" $ do
        it "1" $ simpleFullLangTest "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60"
                                    "parse-time(\"2022-01-01 00:00:00 UTC\")"


 describe "T9" $ do
   describe "Parser" $ do
        it "1" $ simpleParserTest "rand" (hiExprFun HiFunRand)
   describe "Evaluator" $ do
        it "1" $ simpleEvaluatorValueInputTest "rand(0, 10)" (hiValueAction $ HiActionRand 0 10)
   describe "Full-Lang-Test" $ do
        it "1" $ simpleFullLangTest "rand" "rand"
        it "2" $ simpleFullLangTest "rand(0, 10)" "rand(0, 10)"

 describe "T10" $ do
   describe "Parser" $ do
        it "1" $ simpleParserTest "echo" (hiExprFun HiFunEcho)

   describe "Evaluator" $ do
        it "1" $ simpleEvaluatorValueInputTest "echo(\"Hello\")" (hiValueAction $ HiActionEcho $ T.pack "Hello")

        it "2" $ simpleEvaluatorValueInputTest "if(true, 1, 1/0)" (hiValueInt 1)
        it "3" $ simpleEvaluatorValueInputTest "if(false, 1/0, 1)" (hiValueInt 1)
        it "4 my" $ simpleEvaluatorErrorInputTest "if (true, 1/0, 1/0)" HiErrorDivideByZero

        it "5" $ simpleEvaluatorValueInputTest "null && 1 / 0" HiValueNull
        it "6" $ simpleEvaluatorValueInputTest "null || 1" (hiValueInt 1)

        it "7" $ simpleEvaluatorValueInputTest "false && 1 / 0" (HiValueBool False)
        it "8" $ simpleEvaluatorValueInputTest "false || 1" (hiValueInt 1)

        it "9" $ simpleEvaluatorValueInputTest "true && 1" (hiValueInt 1)
        it "10" $ simpleEvaluatorValueInputTest "true || 1 / 0" (HiValueBool True)
   describe "Full-Lang-Test" $ do
        it "1" $ simpleFullLangTest "echo" "echo"
        it "2" $ simpleFullLangTest "echo(\"Hello\")" "echo(\"Hello\")"

        it "3" $ simpleFullLangTest "\"Hello\"(0) || \"Z\"" "\"H\""
        it "4" $ simpleFullLangTest "\"Hello\"(99) || \"Z\"" "\"Z\""

        it "5" $ simpleFullLangTest "if(2 == 2, 1, echo(\"WTF\")!!)" "1"
        it "6" $ simpleFullLangTest "true || echo(\"Don't do this\")!" "true"

        it "7" $ simpleFullLangTest "false && echo(\"Don't do this\")!" "false"
        it "8" $ simpleFullLangEvalErrorTest "[# 00 ff #] && 1!" HiErrorInvalidArgument


 describe "T11" $ do
   describe "Parser" $ do
        it "1" $ simpleParserTest "count" (hiExprFun HiFunCount)
        it "2" $ simpleParserTest "keys" (hiExprFun HiFunKeys)
        it "3" $ simpleParserTest "values" (hiExprFun HiFunValues)
        it "4" $ simpleParserTest "invert" (hiExprFun HiFunInvert)

        it "5" $ simpleParserTest "{ \"width\": 120, \"height\": 80 }" (HiExprDict $ mapToExprs
                                                                       [(hiValueString "width", hiValueInt 120),
                                                                        (hiValueString "height", hiValueInt 80)])
        it "6" $ simpleParserTest "{ 1: true, 3: true, 4: false }" (HiExprDict $ mapToExprs
                                                                    [(hiValueInt 1, HiValueBool True),
                                                                     (hiValueInt 3, HiValueBool True),
                                                                     (hiValueInt 4, HiValueBool False)])

        it "7 dot" $ simpleParserTest "{ \"width\": 120, \"height\": 80 }.width" (HiExprApply (HiExprDict $ mapToExprs
                                                 [(hiValueString "width", hiValueInt 120),
                                                  (hiValueString "height", hiValueInt 80)])
                                                  [HiExprValue $ hiValueString "width"])
        it "8 faq" $ simpleParserTest "add.hello-world" (HiExprApply (hiExprFun HiFunAdd)
                                                                      [HiExprValue $ hiValueString "hello-world"])
        --ws (not testing in tests)
        it "9 my ws test" $ simpleParserEquivalenceTest "{         }" "{}"

   describe "Parser hard!!!" $ do
        -- (.), (!), (f())
        it "1 faq associativity unary operators" $ simpleParserEquivalenceTest
                                                           "{ \"width\": 120, \"height\": 80 }.a.b"
                                                           "({ \"width\": 120, \"height\": 80 }.a).b"
        it "2 faq associativity unary operators" $ simpleParserEquivalenceTest
                                                           "{ \"width\": 120, \"height\": 80 }.a(\"hello\").b"
                                                           "(({ \"width\": 120, \"height\": 80 }.a)(\"hello\")).b"
        it "3 faq associativity unary operators" $ simpleParserEquivalenceTest
                                                           "{ \"width\": 120, \"height\": 80 }.a(\"hello\")!"
                                                           "(({ \"width\": 120, \"height\": 80 }.a)(\"hello\")) !"

   describe "Evaluator" $ do
        it "1" $ simpleEvaluatorValueInputTest "{ \"width\": 120, \"height\": 80 }(\"width\")"
                                               (hiValueInt 120)
        it "2" $ simpleEvaluatorValueInputTest "keys({ \"width\": 120, \"height\": 80 })" (HiValueList (S.fromList
                                                                      [hiValueString "height",
                                                                       hiValueString "width"]))
        it "3" $ simpleEvaluatorValueInputTest "values({ \"width\": 120, \"height\": 80 })" (HiValueList (S.fromList
                                                                              [hiValueInt 80,
                                                                               hiValueInt 120]))
        it "4" $ simpleEvaluatorValueInputTest "count(\"XXXOX\")" (hiValueDict
                                                                   [(hiValueString "O", hiValueInt 1),
                                                                    (hiValueString "X", hiValueInt 4)])
        it "5" $ simpleEvaluatorValueInputTest "count([# 58 58 58 4f 58 #])" (hiValueDict
                                                                   [(hiValueInt 79, hiValueInt 1),
                                                                    (hiValueInt 88, hiValueInt 4)])
        it "6" $ simpleEvaluatorValueInputTest "count([true, true, false, true])" (hiValueDict
                                                                           [(HiValueBool False, hiValueInt 1),
                                                                            (HiValueBool True, hiValueInt 3)])
        it "7" $ simpleEvaluatorValueInputTest "invert({ \"x\": 1, \"y\" : 2, \"z\": 1 })" (hiValueDict
                                                   [(hiValueInt 1,
                                                      HiValueList . S.fromList $ [hiValueString "z", hiValueString "x"]),
                                                    (hiValueInt 2,
                                                      HiValueList . S.fromList $ [hiValueString "y"])])
        it "8 faq" $ simpleEvaluatorErrorInputTest "reverse.to-upper.hello" HiErrorInvalidArgument

   describe "Full-Lang-Test" $ do
        it "1" $ simpleFullLangTest "count(\"Hello World\").o" "2"
        it "2" $ simpleFullLangTest "invert(count(\"big blue bag\"))"
                                    "{1: [\"u\", \"l\", \"i\", \"e\", \"a\"], 2: [\"g\", \" \"], 3: [\"b\"]}"
        it "3" $ simpleFullLangTest "fold(add, values(count(\"Hello, World!\")))" "13"
        it "4 faq" $ simpleFullLangTest "if(true, { \"width\" : 1 }, 1+1).width" "1"
        it "5 faq" $ simpleFullLangTest "{ \"A\" : 1 }.B" "null"
