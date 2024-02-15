module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Meta = Meta Int Int
  deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                         = 0
tsize (Branch (Meta size _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                          = 0
tdepth (Branch (Meta _ depth) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ l val r)
  | val == a = True
  | val < a = tmember a r
  | otherwise = tmember a l

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l val r = Branch (Meta (tsize l + tsize r + 1) (max (tdepth l) (tdepth r) + 1)) l val r

--AVL (taken from wikipedia)
heightBalance :: Tree a -> Tree a
heightBalance (Branch _ l aVal (Branch (Meta _ db) c bVal r))
  | db - tdepth l == 2 && (tdepth c <= tdepth r) = mkBranch a bVal r -- small left
  where
    a = mkBranch l aVal c
heightBalance (Branch _ (Branch (Meta _ db) l bVal c) aVal r)
  | db - tdepth r == 2 && (tdepth c <= tdepth l) = mkBranch l bVal a -- small right
  where
    a = mkBranch c aVal r
heightBalance (Branch _ l aVal (Branch (Meta _ db) (Branch (Meta _ dc) m cVal n) bVal r))
  | db - tdepth l == 2 && (dc > tdepth r) = mkBranch a cVal b -- big left
  where
    a = mkBranch l aVal m
    b = mkBranch n bVal r
heightBalance (Branch _ (Branch (Meta _ db) l bVal (Branch (Meta _ dc) m cVal n)) aVal r)
  | db - tdepth r == 2 && (dc > tdepth l) = mkBranch b cVal a -- big right
  where
    a = mkBranch n aVal r
    b = mkBranch l bVal m
heightBalance tree = tree

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a tree@(Branch _ l val r)
  | val == a = tree
  | val < a = heightBalance $ mkBranch l val (tinsert a r)
  | otherwise = heightBalance $ mkBranch (tinsert a l) val r

tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf
