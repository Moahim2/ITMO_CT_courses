import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Natural (Natural)
import HW1.T2
import HW1.T3
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Natural where
  arbitrary = fromInteger <$> arbitrarySizedNatural

main :: IO ()
main = hspec $ do
  describe "HW.T2" $ do
    it "toValue-fromValue" $
      property $ \a -> nToNum (nFromNatural a) == a
    it "+" $
      property $ \a b -> nToNum (nFromNatural a `nplus` nFromNatural b) == (a + b)
    it "*" $
      property $ \a b -> nToNum (nFromNatural a `nmult` nFromNatural b) == (a * b)
    it "-" $
      property $ \a b ->
        if a > b
          then nToNum (fromJust (nFromNatural a `nsub` nFromNatural b)) == (a - b)
          else nToNum (fromMaybe Z (nFromNatural a `nsub` nFromNatural b)) == (0 :: Int)
    it "<=>" $
      property $ \a b -> case nFromNatural a `ncmp` nFromNatural b of
        EQ -> a == b
        LT -> a < b
        GT -> a > b
    it "even" $
      property $ \a -> nEven (nFromNatural a) == even a
    it "odd" $
      property $ \a -> nOdd (nFromNatural a) == odd a
    it "div" $
      property $ \a b -> (b == 0) || nToNum (nFromNatural a `ndiv` nFromNatural b) == (a `div` b)
    it "mod" $
      property $ \a b -> (b == 0) || (nToNum (nFromNatural a `ndiv` nFromNatural b) == (a `div` b))

  describe "HW.T3" $ do
    it "insertFirst" $
      property $ \a ->
        let tree = tinsert (a :: Int) Leaf
         in tsize tree == 1
              && tdepth tree == 1
              && tmember a tree
    it "fromListOne" $
      property $ \a ->
        let tree = tFromList [a :: Int]
         in tsize tree == 1
              && tdepth tree == 1
              && tmember a tree
    it "fromRandomList" $
      property $ \x ->
        let list = (x :: [Int])
            tree = tFromList list
            uniqList = nub list
         in tsize tree == length uniqList
              && containsElements tree list
              && goodMetaInformation tree
    it "testBalanced" $
      property $ \x ->
        let list = (x :: [Int])
            tree = tFromList list
            uniqList = nub list
         in tsize tree == length uniqList
              && containsElements tree list
              && goodMetaInformation tree
              && goodBalance tree

containsElements :: Ord a => Tree a -> [a] -> Bool
containsElements _ []      = True
containsElements Leaf _    = False
containsElements tree list = tmember (head list) tree && containsElements tree (tail list)

goodMetaInformation :: Tree a -> Bool
goodMetaInformation Leaf = True
goodMetaInformation t@(Branch _ l _ r) =
  tsize t == (tsize l + tsize r + 1)
    && tdepth t == (max (tdepth l) (tdepth r) + 1)
    && goodMetaInformation l
    && goodMetaInformation r

goodBalance :: Tree a -> Bool
goodBalance Leaf = True
goodBalance (Branch _ l _ r) =
  abs (tdepth l - tdepth r) < 2 && goodBalance l && goodBalance r

-- did not test the orientation of the tree
