module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState fun (S getAnn) = S $ mapAnnotated fun . getAnn

wrapState :: a -> State s a
wrapState val = S $ \s -> val :# s

joinState :: State s (State s a) -> State s a
joinState (S getExternalS) = S $ \s0 ->
  let ((S getInnerS) :# s1) = getExternalS s0
  in getInnerS s1

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (S getFunAnn) <*> (S getValAnn) = S $ \s0 ->
    let (fun :# s1) = getFunAnn s0
        (val :# s2) = getValAnn s1
    in fun val :# s2

instance Monad (State s) where
  m >>= f = joinState $ f <$> m

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) = makeBinExpr Add
  (-) = makeBinExpr Sub
  (*) = makeBinExpr Mul
  abs = makeUnExpr Abs
  signum = makeUnExpr Sgn
  fromInteger = Val . fromInteger

instance Fractional Expr where
  (/) = makeBinExpr Div
  fromRational = Val . fromRational

makeBinExpr :: (a -> b -> Prim Expr) -> a -> b -> Expr
makeBinExpr primConstr a b = Op $ primConstr a b

makeUnExpr :: (a -> Prim Expr) -> a -> Expr
makeUnExpr primConstr val = Op $ primConstr val

eval :: Expr -> State [Prim Double] Double
eval (Val val)      = pure val
eval (Op prim)      = evalPrim prim
  where
    evalPrim (Add a b) = calcBinPrim Add (+) a b
    evalPrim (Sub a b) = calcBinPrim Sub (-) a b
    evalPrim (Mul a b) = calcBinPrim Mul (*) a b
    evalPrim (Div a b) = calcBinPrim Div (/) a b
    evalPrim (Abs val) = calcUnPrim Abs abs val
    evalPrim (Sgn val) = calcUnPrim Sgn signum val

calcBinPrim :: (Double -> Double -> Prim Double) -> (Double -> Double -> a) -> Expr -> Expr -> State [Prim Double] a
calcBinPrim primConstr op exprA exprB = do
  a <- eval exprA
  b <- eval exprB
  modifyState (primConstr a b :)
  return $ op a b

calcUnPrim :: (Double -> Prim Double) -> (Double -> a) -> Expr -> State [Prim Double] a
calcUnPrim primConstr op expr = do
  val <- eval expr
  modifyState (primConstr val :)
  return $ op val
