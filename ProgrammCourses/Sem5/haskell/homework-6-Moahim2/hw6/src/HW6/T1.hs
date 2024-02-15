module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (MonadConc, MonadSTM, STM, atomically)
import Control.Concurrent.Classy.STM (TArray, TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (forM_, when, unless)
import Data.Array.MArray (getBounds, getElems, newArray, readArray, writeArray)
import Data.Hashable (Hashable, hash)
import GHC.Base (liftM2)

-- |Constant for constructing new map (start count of buckets).
initCapacity :: Int
initCapacity = 16

-- |Constant for checking if a resizing is needed now.
loadFactor :: Double
loadFactor = 0.75

-- |Constant for resizing (change count of buckets) after changing size operations.
resizeConst :: Double
resizeConst = 2.5 -- may be any const > 1 (for working asymptotic)

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

-- |Concurrent hash-map.
data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v) -- ^ current array of all buckets
  , chtSize    :: TVar stm Int                    -- ^ current size (count of elements)
  }

-- |Init new map with @initCapacity@ count of buckets.
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ liftM2 CHT (newTVar =<< newArray (calcRange initCapacity) []) $ newTVar 0

-- |Try to get value on key.
getCHT
 :: ( MonadConc m
    , Eq k
    , Hashable k
    )
 => k               -- ^ key
 -> CHT (STM m) k v -- ^ map
 -> m (Maybe v)     -- ^ value if map contains key else Nothing.
getCHT key cht = atomically $ do
  BI { kBucket = bucket } <- getBucketInfo key cht
  return $ lookup key bucket

-- |Try to put value with key.
--  Change count of buckets if after put: chtSize >= capacity * loadFactor.
putCHT
 :: ( MonadConc m
    , Eq k
    , Hashable k
    )
 => k               -- ^ key
 -> v               -- ^ value
 -> CHT (STM m) k v -- ^ map.
 -> m ()
putCHT key val cht = atomically $ do
  BI { kBucket = oldBucket
     , kBucketIndex = bucketIndex
     , originalArr = arr
     , countOfBuckets = capacity } <- getBucketInfo key cht

-- write
  let (newBucket, isSwap) = putImpl [] oldBucket key val
  writeArray arr bucketIndex newBucket
  let sizeVar = chtSize cht
  unless isSwap $ modifyTVar sizeVar (+ 1)

--resize
  size <- readTVar sizeVar
  when (checkNeedResize size capacity) $ do
      let newCapacity = calcNewCapacity capacity
      buckets <- getElems arr
      writeTVar (chtBuckets cht) =<< newArray (calcRange newCapacity) []
      newArr <- readTVar . chtBuckets $ cht
      forM_ buckets $ \bucket ->
        forM_ bucket $ \(k, v) -> do
          let bi = calcBucketIndex k newCapacity
          curBucket <- readArray newArr bi
          writeArray newArr bi $ (k, v) : curBucket

    where
      putImpl h (p@(k, _) : xs) newKey newVal
        | k == newKey  = (h ++ ((newKey, newVal) : xs), True)
        | otherwise = putImpl (p : h) xs newKey newVal
      putImpl h [] newKey newVal = ((newKey, newVal) : h, False)

      checkNeedResize curSize curCapacity = fromIntegral curSize >= fromIntegral curCapacity * loadFactor
      calcNewCapacity curCapacity = ceiling $ fromIntegral curCapacity * resizeConst

-- | Get size (count of all elements) of map.
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT = atomically . readTVar . chtSize

----------------------------------------------------------------------------
-- Common helpers

-- |Calc range (0, n-1).
calcRange :: (Num a, Num b) => b -> (a, b)
calcRange capacity = (0, capacity - 1)

-- |Calc index of bucket for key according to the rule: hash(k) % capacity.
calcBucketIndex :: Hashable a => a -> Int -> Int
calcBucketIndex k c = hash k `mod` c

-- |A private structure for convenient transmission of information about
--  the requested batch and the entire batch array.
data BucketInfo stm k v = BI
  { kBucket        :: Bucket k v           -- ^ required butch
  , kBucketIndex   :: Int                  -- ^ index of required butch
  , originalArr    :: BucketsArray stm k v -- ^ all array of butch
  , countOfBuckets :: Int                  -- ^ capacity of array of butch.
  }

-- |Return BucketInfo about bucket for this key.
getBucketInfo
  :: ( MonadSTM stm
     , Hashable k
     )
  => k                        -- ^ key
  -> CHT stm k v              -- ^ map
  -> stm (BucketInfo stm k v) -- ^ structure with required info.
getBucketInfo key cht = do
  array <- readTVar . chtBuckets $ cht
  (l, r) <- getBounds array
  let capacity = r - l + 1 -- it seems like 'l' is always == 0 but it's probably better this way.
  let curBucketIndex = calcBucketIndex key capacity
  curBucket <- readArray array curBucketIndex
  return $ BI
   { kBucket = curBucket
   , kBucketIndex = curBucketIndex
   , originalArr = array
   , countOfBuckets = capacity
   }
