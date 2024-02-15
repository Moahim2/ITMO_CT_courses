{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module HW5.Evaluator
  ( eval
  ) where

import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction),
                 HiValue (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompress,
                               defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except
import Data.Semigroup (stimes)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (addUTCTime, diffUTCTime)
import GHC.Exts (IsList, Item, fromList, toList)
import GHC.Real (Ratio ((:%)))
import Text.Read (readMaybe)

-- |Evaluates expr and in HiMonad Either context return evaluated value if evaluation was ok else return HiError.
-- As part of the evaluation, the priority of errors is as follows at the current version:
--    - 1) HiErrorInvalidFunction
--    - 2) HiErrorArityMismatch
--    - 3) HiErrorInvalidArgument
--    - 4) HiErrorDivideByZero
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalToExceptT

-- |Evaluates expr to ExceptT context.
evalToExceptT :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalToExceptT (HiExprApply func args) = do
  evalApplier <- evalToExceptT func
  case checkArity evalApplier (length args) of
      Nothing      -> applyArgs evalApplier args
      Just hiError -> throwError hiError
evalToExceptT (HiExprValue hiValue) = return hiValue
evalToExceptT (HiExprRun expr) = do
  evalExpr <- evalToExceptT expr
  case evalExpr of
    HiValueAction action -> lift $ runAction action
    _                    -> throwError HiErrorInvalidArgument
evalToExceptT (HiExprDict arrPairs) = do
  evalArgs <- mapM (\(k, a) -> liftM2 (,) (evalToExceptT k) (evalToExceptT a)) arrPairs
  return . HiValueDict . M.fromList $ evalArgs

-- |Check arity for all entities of HiValue and
-- return Nothing if correct arity
-- else return HiError:
-- InvalidFunction if to value we cannot apply args
-- else ArityMismatch.
checkArity :: HiValue -> Int -> Maybe HiError
checkArity (HiValueNumber _) _                     = return HiErrorInvalidFunction
checkArity (HiValueBool _) _                       = return HiErrorInvalidFunction
checkArity HiValueNull _                           = return HiErrorInvalidFunction
checkArity (HiValueAction _) _                     = return HiErrorInvalidFunction
checkArity (HiValueTime _) _                       = return HiErrorInvalidFunction
checkArity (HiValueString _) 1                     = Nothing
checkArity (HiValueString _) 2                     = Nothing
checkArity (HiValueList _) 1                       = Nothing
checkArity (HiValueList _) 2                       = Nothing
checkArity (HiValueBytes _) 1                      = Nothing
checkArity (HiValueBytes _) 2                      = Nothing
checkArity (HiValueDict _) 1                       = Nothing
checkArity (HiValueFunction HiFunDiv) 2            = Nothing
checkArity (HiValueFunction HiFunMul) 2            = Nothing
checkArity (HiValueFunction HiFunAdd) 2            = Nothing
checkArity (HiValueFunction HiFunSub) 2            = Nothing
checkArity (HiValueFunction HiFunNot) 1            = Nothing
checkArity (HiValueFunction HiFunAnd) 2            = Nothing
checkArity (HiValueFunction HiFunOr) 2             = Nothing
checkArity (HiValueFunction HiFunLessThan) 2       = Nothing
checkArity (HiValueFunction HiFunGreaterThan) 2    = Nothing
checkArity (HiValueFunction HiFunEquals) 2         = Nothing
checkArity (HiValueFunction HiFunNotLessThan) 2    = Nothing
checkArity (HiValueFunction HiFunNotGreaterThan) 2 = Nothing
checkArity (HiValueFunction HiFunNotEquals) 2      = Nothing
checkArity (HiValueFunction HiFunIf) 3             = Nothing
checkArity (HiValueFunction HiFunLength) 1         = Nothing
checkArity (HiValueFunction HiFunToUpper) 1        = Nothing
checkArity (HiValueFunction HiFunToLower) 1        = Nothing
checkArity (HiValueFunction HiFunReverse) 1        = Nothing
checkArity (HiValueFunction HiFunTrim) 1           = Nothing
checkArity (HiValueFunction HiFunList) _           = Nothing
checkArity (HiValueFunction HiFunRange) 2          = Nothing
checkArity (HiValueFunction HiFunFold) 2           = Nothing
checkArity (HiValueFunction HiFunPackBytes) 1      = Nothing
checkArity (HiValueFunction HiFunUnpackBytes) 1    = Nothing
checkArity (HiValueFunction HiFunEncodeUtf8) 1     = Nothing
checkArity (HiValueFunction HiFunDecodeUtf8) 1     = Nothing
checkArity (HiValueFunction HiFunZip) 1            = Nothing
checkArity (HiValueFunction HiFunUnzip) 1          = Nothing
checkArity (HiValueFunction HiFunSerialise) 1      = Nothing
checkArity (HiValueFunction HiFunDeserialise) 1    = Nothing
checkArity (HiValueFunction HiFunRead) 1           = Nothing
checkArity (HiValueFunction HiFunWrite) 2          = Nothing
checkArity (HiValueFunction HiFunMkDir) 1          = Nothing
checkArity (HiValueFunction HiFunChDir) 1          = Nothing
checkArity (HiValueFunction HiFunParseTime) 1      = Nothing
checkArity (HiValueFunction HiFunRand) 2           = Nothing
checkArity (HiValueFunction HiFunEcho) 1           = Nothing
checkArity (HiValueFunction HiFunCount) 1          = Nothing
checkArity (HiValueFunction HiFunKeys) 1           = Nothing
checkArity (HiValueFunction HiFunValues) 1         = Nothing
checkArity (HiValueFunction HiFunInvert) 1         = Nothing
checkArity _ _                                     = return HiErrorArityMismatch

-- |Applies arguments on value (separates the regular and lazy options).
applyArgs :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
applyArgs (HiValueFunction HiFunIf) [a, br1, br2] = calculateLazyApplying a $ \case
  HiValueBool cond -> evalToExceptT $ if cond then br1 else br2
  _                -> throwError HiErrorInvalidArgument
applyArgs (HiValueFunction HiFunAnd) [a, b] = calcLogicBinOp id a b
applyArgs (HiValueFunction HiFunOr) [a, b] = calcLogicBinOp not a b
applyArgs evalApplier args = do
 evalArgs <- mapM evalToExceptT args
 calculateApplying evalApplier evalArgs

-- |Lazy calculated (&&) and (||) on rules from faq.
calcLogicBinOp :: HiMonad m => (Bool -> Bool) -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
calcLogicBinOp pr l r = calculateLazyApplying l $ \leftVal ->
 if pr $ leftVal `elem` [HiValueNull, HiValueBool False]
  then return leftVal
  else evalToExceptT r

-- |Lazy applies first argument of expr.
calculateLazyApplying :: HiMonad m => HiExpr -> (HiValue -> ExceptT HiError m b) -> ExceptT HiError m b
calculateLazyApplying firstArg activity = do
  evalArg <- evalToExceptT firstArg
  activity evalArg

-- All evaluates are divided into sections according to the type of object to which the arguments are applied and
-- the signature of the arguments (types and quantity).
-- |Main final calculation of the application.
calculateApplying :: Monad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
calculateApplying (HiValueFunction hiFun) [HiValueNumber a, HiValueNumber b]                                 --2 numbers
  | HiFunDiv            <- hiFun
  , b == 0                                  = throwError HiErrorDivideByZero
  | HiFunDiv            <- hiFun            = calculateBinOp HiValueNumber (/) a b
  | HiFunMul            <- hiFun            = calculateBinOp HiValueNumber (*) a b
  | HiFunAdd            <- hiFun            = calculateBinOp HiValueNumber (+) a b
  | HiFunSub            <- hiFun            = calculateBinOp HiValueNumber (-) a b
  | HiFunRange          <- hiFun            = return . HiValueList . fromList . take (floor (b - a) + 1) $
                                               [HiValueNumber n | n <- [a..]]
  | HiFunRand           <- hiFun            = calculateWithCheckOnIntTwoArg a b $
                                            calculateBinOp HiValueAction HiActionRand
calculateApplying (HiValueFunction hiFun) [a, b]                                                            --2 any args
  | HiFunLessThan       <- hiFun            = calculateBinOp HiValueBool (<) a b
  | HiFunGreaterThan    <- hiFun            = calculateBinOp HiValueBool (>) a b
  | HiFunEquals         <- hiFun            = calculateBinOp HiValueBool (==) a b
  | HiFunNotLessThan    <- hiFun            = calculateBinOp HiValueBool (>=) a b
  | HiFunNotGreaterThan <- hiFun            = calculateBinOp HiValueBool (<=) a b
  | HiFunNotEquals      <- hiFun            = calculateBinOp HiValueBool (/=) a b
calculateApplying (HiValueFunction HiFunNot) [HiValueBool a] = return . HiValueBool $ not a                     --1 bool
calculateApplying (HiValueFunction hiFun) [HiValueString a, HiValueString b]                                 --2 strings
  | HiFunDiv            <- hiFun            = calculateBinOp HiValueString T.append (a `T.snoc` '/') b
  | HiFunAdd            <- hiFun            = lAdd a b HiValueString
  | HiFunWrite          <- hiFun            = return $ HiValueAction $ HiActionWrite (T.unpack a) (encodeUtf8 b)
calculateApplying (HiValueFunction HiFunAdd) [HiValueList a, HiValueList b] = lAdd a b HiValueList                 --add
calculateApplying (HiValueFunction HiFunAdd) [HiValueBytes a, HiValueBytes b] = lAdd a b HiValueBytes
calculateApplying (HiValueFunction HiFunMul) [HiValueString text, HiValueNumber b] = lMul text b HiValueString     --mul
calculateApplying (HiValueFunction HiFunMul) [HiValueList list, HiValueNumber b] = lMul list b HiValueList
calculateApplying (HiValueFunction HiFunMul) [HiValueBytes bytes, HiValueNumber b] = lMul bytes b HiValueBytes
calculateApplying (HiValueFunction hiFun) [HiValueString text]                                                --1 string
  | HiFunLength         <- hiFun            = calculateUnOp HiValueNumber (toRational . T.length) text
  | HiFunReverse        <- hiFun            = calculateUnOp HiValueString T.reverse text
  | HiFunToUpper        <- hiFun            = calculateUnOp HiValueString T.toUpper text
  | HiFunToLower        <- hiFun            = calculateUnOp HiValueString T.toLower text
  | HiFunTrim           <- hiFun            = calculateUnOp HiValueString T.strip text
  | HiFunEncodeUtf8     <- hiFun            = calculateUnOp HiValueBytes encodeUtf8 text
  | HiFunRead           <- hiFun            = makeDirAction HiActionRead
  | HiFunMkDir          <- hiFun            = makeDirAction HiActionMkDir
  | HiFunChDir          <- hiFun            = makeDirAction HiActionChDir
  | HiFunParseTime      <- hiFun            = return $ case readMaybe (T.unpack text) of
                                                Nothing   -> HiValueNull
                                                Just time -> HiValueTime time
  | HiFunEcho           <- hiFun            = makeActionOneArg HiActionEcho text
  | HiFunCount          <- hiFun            = lCount text $ HiValueString . fromList . (: [])
    where
      makeDirAction actionType = makeActionOneArg actionType $ T.unpack text
calculateApplying (HiValueFunction hiFun) [HiValueList list]                                                    --1 list
  | HiFunLength         <- hiFun            = calculateUnOp HiValueNumber (toRational . S.length) list
  | HiFunReverse        <- hiFun            = calculateUnOp HiValueList S.reverse list
  | HiFunPackBytes      <- hiFun            = do
                                            ints <- mapM getValIfByteIntElseThrowInvalidArg $ toList list
                                            calculateUnOp HiValueBytes (BS.pack . fmap fromInteger) ints
  | HiFunCount          <- hiFun            = lCount list id
calculateApplying (HiValueFunction hiFun) [HiValueBytes bytes]                                                 --1 bytes
  | HiFunLength         <- hiFun            = calculateUnOp HiValueNumber (toRational . BS.length) bytes
  | HiFunReverse        <- hiFun            = calculateUnOp HiValueBytes BS.reverse bytes
  | HiFunUnpackBytes    <- hiFun            = calculateUnOp HiValueList
                                                (fromList . fmap (HiValueNumber . toRational) . BS.unpack) bytes
  | HiFunDecodeUtf8     <- hiFun            = case decodeUtf8' bytes of
                                                    Left _     -> return HiValueNull
                                                    Right text -> return . HiValueString $ text
  | HiFunZip            <- hiFun            = compressImpl $
                                            compressWith (defaultCompressParams { compressLevel = bestCompression })
  | HiFunUnzip          <- hiFun            = compressImpl decompress
  | HiFunDeserialise    <- hiFun            = return $ case deserialiseOrFail (BSL.fromStrict bytes) of
                                                    Left _    -> HiValueNull
                                                    Right obj -> obj
  | HiFunCount          <- hiFun            = lCount bytes $ HiValueNumber . toRational
    where
      compressImpl operation = calculateUnOp (HiValueBytes . BSL.toStrict) operation (BSL.fromStrict bytes)
calculateApplying hiValue [HiValueNumber a]                                                                   --1 number
  | HiValueString text         <- hiValue   = lIndex text a $ HiValueString . fromList . (: [])
  | HiValueList list           <- hiValue   = lIndex list a id
  | HiValueBytes bytes         <- hiValue   = lIndex bytes a $ HiValueNumber . toRational
calculateApplying (HiValueFunction HiFunFold) [f@(HiValueFunction _), HiValueList arr] = case checkArity f 2 of --f + list
                        Nothing -> foldl (\a b -> do
                                      valueA <- a
                                      valueB <- b
                                      if valueA == HiValueNull
                                        then return valueB
                                        else calculateApplying f [valueA, valueB]) (return HiValueNull) $ return <$> arr
                        Just _  -> throwError HiErrorInvalidArgument
calculateApplying (HiValueFunction HiFunAdd) [HiValueTime time, HiValueNumber num] =                     --time + number
                                            calculateBinOp HiValueTime addUTCTime (fromRational num) time
calculateApplying (HiValueFunction HiFunSub) [HiValueTime time1, HiValueTime time2] =                      --time + time
                                            calculateBinOp (HiValueNumber . toRational) diffUTCTime time1 time2
calculateApplying (HiValueFunction hiFun) [HiValueDict dict]                                                    --1 dict
  | HiFunKeys                   <- hiFun    = calculateUnOp (HiValueList . fromList) M.keys dict
  | HiFunValues                 <- hiFun    = calculateUnOp (HiValueList . fromList) M.elems dict
  | HiFunInvert                 <- hiFun    = calculateUnOp HiValueDict (M.map $ HiValueList . fromList) $
                                    M.fromListWith (++) [(val, [key]) | (key, val) <- M.toList dict]
calculateApplying hiValue [arg]                                                                              --1 any arg
  | HiValueFunction HiFunSerialise  <- hiValue    = calculateUnOp (HiValueBytes . BSL.toStrict) serialise arg
  | HiValueDict dict                <- hiValue    = return $ case M.lookup arg dict of
                                                      Nothing -> HiValueNull
                                                      Just a  -> a
calculateApplying hiValue arr                                                                                  --any arr
  | HiValueFunction HiFunList   <- hiValue  = calculateUnOp HiValueList fromList arr
  | HiValueString text          <- hiValue  = lSlice text arr HiValueString
  | HiValueList list            <- hiValue  = lSlice list arr HiValueList
  | HiValueBytes bytes          <- hiValue  = lSlice bytes arr HiValueBytes
calculateApplying _ _                       = throwError HiErrorInvalidArgument                    --other cases are bad

----------------------------------------------------------------------------
-- Helpers for making calculations.

calculateWithCheckOnIntOneArg :: Monad m => Rational -> (Int -> ExceptT HiError m b) -> ExceptT HiError m b
calculateWithCheckOnIntOneArg a fun = do
  intA <- getValIfIntElseThrowInvalidArg a
  fun intA

calculateWithCheckOnIntTwoArg :: Monad m => Rational -> Rational -> (Int -> Int -> ExceptT HiError m b)
                              -> ExceptT HiError m b
calculateWithCheckOnIntTwoArg a b fun = do
  intA <- getValIfIntElseThrowInvalidArg a
  intB <- getValIfIntElseThrowInvalidArg b
  fun intA intB

-- |Checks that val is integer else throw HiErrorInvalidArgument.
getValIfIntElseThrowInvalidArg :: Monad m => Rational -> ExceptT HiError m Int
getValIfIntElseThrowInvalidArg (a :% b) = let (q, r) = quotRem a b in
 case r of
   0 -> return $ fromInteger q
   _ -> throwError HiErrorInvalidArgument

calculateUnOp :: Monad m => (a1 -> a2) -> (t -> a1) -> t -> m a2
calculateUnOp valueType op a = return . valueType $ op a

calculateBinOp :: Monad m => (a1 -> a2) -> (t1 -> t2 -> a1) -> t1 -> t2 -> m a2
calculateBinOp valueType op a = calculateUnOp valueType $ op a

-- |Return integer value if arg is byte else throws HiErrorInvalidArgument.
getValIfByteIntElseThrowInvalidArg :: Monad m => HiValue -> ExceptT HiError m Integer
getValIfByteIntElseThrowInvalidArg (HiValueNumber num) = do
                                                numInt <- getValIfIntElseThrowInvalidArg num
                                                if numInt < 0 || numInt > 255
                                                  then throwError HiErrorInvalidArgument
                                                  else return . toInteger $ numInt
getValIfByteIntElseThrowInvalidArg _ = throwError HiErrorInvalidArgument

-- | Helper for creating HiValueAction with HiAction with one arg type.
makeActionOneArg :: Monad m => (a -> HiAction) -> a -> m HiValue
makeActionOneArg actionType arg = return . HiValueAction . actionType $ arg

----------------------------------------------------------------------------
-- Helpers for common operations with list objects (for text, list, bytes)

-- |Concat of two list object (work for any isList).
lAdd :: (Monad m, IsList a) => a -> a -> (a -> HiValue) -> ExceptT HiError m HiValue
lAdd listI1 listI2 constructor = calculateBinOp (constructor . fromList) (++) (toList listI1) (toList listI2)

-- |Repeats the list object n times (work for any isList).
lMul :: (Monad m, IsList a) => a -> Rational -> (a -> HiValue) -> ExceptT HiError m HiValue
lMul listI a constructor = calculateWithCheckOnIntOneArg a $
 calculateBinOp (constructor . fromList) (flip stimes) $ toList listI

-- |Gets slice on an arbitrary isList object.
lSlice :: (Monad m, IsList l) => l -> [HiValue] -> (l -> HiValue) -> ExceptT HiError m HiValue
lSlice listI arr constructor
 | [HiValueNumber a, HiValueNumber b] <- arr = calculateWithCheckOnIntTwoArg a b $ calculateBinOp constructor substring
 | [HiValueNull, b]     <- arr = calculateApplying (constructor listI) [HiValueNumber (toRational (0::Int)), b]
 | [a, HiValueNull]     <- arr = calculateApplying (constructor listI) [a, HiValueNumber (toRational len)]
 | otherwise = throwError HiErrorInvalidArgument
    where
      list = toList listI
      len = length list
      substring l r = fromList $
        if (r >= 0 && l > r) || l >= len
          then []
          else let a = l `mod` len
                   b = if r >= len
                        then len
                        else r `mod` len
               in (b - a) `take` (a `drop` list)

-- |Gets index on an arbitrary isList object.
lIndex :: (Monad m, IsList a) => a -> Rational -> (Item a -> HiValue) -> ExceptT HiError m HiValue
lIndex listI index constructor = calculateWithCheckOnIntOneArg index $ \c -> return $ 
  case getI (toList listI) c of
    Nothing -> HiValueNull
    Just x  -> constructor x
    where
      getI [] _       = Nothing
      getI (x : xs) n
        | n < 0       = Nothing
        | n == 0      = Just x
        | otherwise   = getI xs $ n - 1
        
-- |Forms map of counts for an arbitrary isList object.
lCount :: (Monad m, IsList a, Ord (Item a)) => a -> (Item a -> HiValue) -> ExceptT HiError m HiValue
lCount listI constrValue = return $ HiValueDict $ M.mapKeys constrValue $ M.map (HiValueNumber . toRational) $
 M.fromListWith (+) $ map (\a -> (a, (1::Int))) $ toList listI
