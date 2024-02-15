module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types

import Control.Monad

newtype ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES getE) = ES $ \s -> mapExcept (mapAnnotated f) (getE s)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState val = ES $ \s -> wrapExcept (val :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES getExternalES) = ES $ \s0 ->
  case getExternalES s0 of
    Success ((ES getInnerES) :# s1) -> getInnerES s1
    Error t                         -> Error t

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> wrapExcept (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState $ f <$> m

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val val) = pure val
eval (Op prim) = evalPrim prim
  where
    evalPrim (Add exprA exprB) = calcBinPrim Add (+) exprA exprB
    evalPrim (Sub exprA exprB) = calcBinPrim Sub (-) exprA exprB
    evalPrim (Mul exprA exprB) = calcBinPrim Mul (*) exprA exprB
    evalPrim (Div exprA exprB) = do
      a <- eval exprA
      b <- eval exprB
      modifyExceptState (Div a b :)
      if b == 0 then throwExceptState DivideByZero
      else return $ (/) a b
    evalPrim (Abs val) = calcUnPrim Abs abs val
    evalPrim (Sgn val) = calcUnPrim Sgn signum val

calcBinPrim :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Expr -> Expr ->
 ExceptState EvaluationError [Prim Double] Double
calcBinPrim primConstr op exprA exprB = do
  a <- eval exprA
  b <- eval exprB
  modifyExceptState (primConstr a b :)
  return $ op a b

calcUnPrim :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
calcUnPrim primConstr op expr = do
  val <- eval expr
  modifyExceptState (primConstr val :)
  return $ op val
