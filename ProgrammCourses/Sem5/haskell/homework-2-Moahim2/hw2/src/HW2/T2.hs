module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr processSymbol ([] :| [])
  where
    processSymbol sym (h :| t)
      | separator == sym = [] :| (h : t)
      | otherwise = (sym : h) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep = foldr1 $ \subStr mainStr -> subStr ++ (sep : mainStr)
