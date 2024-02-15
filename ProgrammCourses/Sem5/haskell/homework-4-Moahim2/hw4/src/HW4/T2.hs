{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Control.Applicative
import Control.Monad
import Numeric.Natural (Natural)

import Data.Char
import Data.Ratio
import HW4.T1 (ExceptState (..))
import HW4.Types

newtype ParseError
  = ErrorAtPos Natural
  deriving (Show, Eq)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) str = case runES es (0, str) of
  Success (v :# _) -> Success v
  Error e          -> Error e

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P . ES $ Error . ErrorAtPos . fst

--get 'max' for laws hold.
instance Alternative Parser where
  empty = parseError
  P (ES runES1) <|> P (ES runES2) = P $ ES $ \st ->
    case runES1 st of
      Error (ErrorAtPos posE1) ->
        case runES2 st of
          Error (ErrorAtPos posE2) -> Error $ ErrorAtPos $ max posE1 posE2
          success                  -> success
      success -> success

-- No methods
instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
 case s of
   [] -> Success (() :# (pos, ""))
   _  -> Error (ErrorAtPos pos)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = P $ ES $ \(pos, s) ->
 case s of
  (c:cs) | predicate c -> Success (c :# (pos + 1, cs))
  _                    -> Error (ErrorAtPos pos)

expectedChar :: Char -> Parser Char
expectedChar ch = satisfy (== ch)

digit :: Parser Char
digit = satisfy isDigit

int :: Parser String
int = some digit

charToInt :: Char -> Integer
charToInt c = fromIntegral $ ord c - ord '0'

strToInteger :: String -> Integer
strToInteger = foldr (\c curN -> curN * 10 + charToInt c) 0 . reverse

--I made a minus just in case there are negative numbers in the tests.
-- (I think there is a difference between a negative number and a unary minus).
number :: Parser Double
number = do
  minus <- optional $ expectedChar '-'
  l <- int
  dot <- optional $ expectedChar '.'
  let sgn = case minus of
       Nothing -> (1::Double)
       Just _  -> -1
  case dot of
    Nothing -> return $ sgn * fromIntegral (strToInteger l)
    Just _  -> do
      r <- int
      return $ sgn * fromRational (strToRational (l ++ r) r)
        where
          -- for a little acceleration (do not go through the length once again or do not use an extra logarithm)
          powTenToLengthOfFracPart fracPart = foldr (\_ cur -> cur * 10) 1 fracPart
          strToRational str fracPart = strToInteger str % powTenToLengthOfFracPart fracPart

--array pairs for potential extensibility
operationConstructor :: [(Char, a -> a -> Prim a)] -> Parser (a -> a -> Prim a)
operationConstructor bijections = foldr1 (<|>) $
 map (\(c, constructor) -> expectedChar c >> return constructor) bijections

makeBinaryExpr :: (a -> a -> Prim Expr) -> a -> a -> Expr
makeBinaryExpr constr e1 e2 = Op $ constr e1 e2

ws :: Parser ()
ws = void . many $ satisfy isSpace

binaryOperationImpl :: Parser Expr -> Parser Expr -> [(Char, Expr -> Expr -> Prim Expr)] -> Parser Expr
binaryOperationImpl parserRule1 parserRule2 bijections = do
  a <- parserRule1
  constr <- operationConstructor bijections
  makeBinaryExpr constr a <$> parserRule2

-- All rules (with modification) from MT course:
-- E -> T (+-) E
-- E -> T
-- T -> F (*/) T
-- T -> F
-- F -> (E)
-- F -> n

-- n
exprVal :: Parser Expr
exprVal = Val <$> number

-- (E)
bracketOperation :: Parser Expr
bracketOperation = do
  _ <- expectedChar '('
  op <- exprE
  _ <- expectedChar ')'
  return op

skipWSAltParser :: Parser Expr -> Parser Expr -> Parser Expr
skipWSAltParser parserRule1 parserRule2 = ws *> (parserRule1 <|> parserRule2) <* ws

--F
exprF :: Parser Expr
exprF = skipWSAltParser exprVal bracketOperation

--T
exprT :: Parser Expr
exprT = skipWSAltParser operationPrior1 exprF
  where
    --(F (*/) T)
    operationPrior1 = binaryOperationImpl exprF exprT [('*', Mul), ('/', Div)]

--E
exprE :: Parser Expr
exprE = skipWSAltParser operationPrior2 exprT
  where
    --(T (+-) E)
    operationPrior2 = binaryOperationImpl exprT exprE [('+', Add), ('-', Sub)]

fullExpr :: Parser Expr
fullExpr = exprE <* pEof

parseExpr :: String -> Except ParseError Expr
parseExpr = runP fullExpr
