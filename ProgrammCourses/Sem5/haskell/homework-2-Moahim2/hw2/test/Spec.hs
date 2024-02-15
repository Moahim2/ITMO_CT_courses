import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..), filter, fromList)
import HW2.T1
import HW2.T2
import HW2.T3
import HW2.T4
import Test.Hspec
import Test.QuickCheck

newtype MInt = MI Int
  deriving (Eq)

instance Semigroup MInt where
  (MI i) <> (MI j) = MI (i + j)

instance Monoid MInt where
    mempty  = MI 0

main :: IO ()
main = hspec $ do
  describe "HW2.T1" $ do
    it "test tfoldr" $
      property $ \x ->
        let list = (x :: [Int])
            tree = tFromList list
            uniqSortList = sort $ nub list
         in uniqSortList == treeToList tree
  describe "HW2.T2" $ do
    it "testSplitOn" $
      splitOn '/' "path/to/file" == "path" :| ["to", "file"] &&
      splitOn '/' "///path/to/file//" == "" :| ["", "", "path", "to", "file", "", ""]
    it "testJoinWith" $
           joinWith '/' (splitOn '/' "path/to/file") == "path/to/file" &&
           joinWith '/' (splitOn '/' "///path/to/file///") == "///path/to/file///" &&
           joinWith '/' (splitOn '/' "pathtofile") == "pathtofile"
  describe "HW2.T3" $ do
    it "mcat simple" $
      mcat [Just "mo", Nothing, Nothing, Just "no", Just "id"] == "monoid"
    it "mcat" $
      property $ \x ->
        let list = (x :: [Int])
            fun el
              | el < 0    = Nothing
              | otherwise = Just (MI el)
            listC = map fun list
            result = sum (Prelude.filter (>= 0) list)
        in mcat listC == MI result
  describe "HW2.T4" $ do
    it "DotString" $
      property $ \x xs ->
        let arrStr = ((x::[Char]) : xs)
            nonEmptyStrings = Data.List.NonEmpty.filter (/= "") (x :| xs)
            expected = joinWith '.' (fromList nonEmptyStrings)
            assertEQDS (DS str1) (DS str2) = str1 == str2
        in
          null nonEmptyStrings || mconcat (map DS arrStr) `assertEQDS` DS expected

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a tree@(Branch _ l val r)
  | val == a = tree
  | val < a = mkBranch l val (tinsert a r)
  | otherwise = mkBranch (tinsert a l) val r

tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l val r = Branch (tsize l + tsize r + 1) l val r


treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []

tsize :: Tree a -> Int
tsize Leaf                = 0
tsize (Branch size _ _ _) = size
