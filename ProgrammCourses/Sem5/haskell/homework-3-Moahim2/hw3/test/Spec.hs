{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}


import HW3.T1
import HW3.T2
import HW3.T3
import Test.Hspec
import Test.QuickCheck

--Option
newtype EqOption a = EqOption (Option a)
  deriving (Show)

instance (Eq a) =>  Eq (EqOption a)
  where
    EqOption None == EqOption None         = True
    EqOption (Some a) == EqOption (Some b) = a == b
    _ == _                                 = False

instance Arbitrary a => Arbitrary (EqOption a) where
  arbitrary = do
    n <- chooseInt (0, 1)
    case n of
      0 -> return $ EqOption None
      _ -> EqOption . Some <$> arbitrary

instance Functor EqOption
  where
    fmap f (EqOption opt) = EqOption (mapOption f opt)

instance Applicative EqOption where
    pure a = EqOption $ wrapOption a
    (<*>) = undefined

instance Monad EqOption where
  (EqOption m) >>= f = EqOption $ joinOption $ (\i -> let (EqOption b) = f i in b) `mapOption` m


--Pair
newtype EqPair a = EqPair (Pair a)
  deriving (Show)

instance (Eq a) =>  Eq (EqPair a)
  where
    EqPair (P a b) == EqPair (P c d) = a == c && b == d

instance Arbitrary a => Arbitrary (EqPair a) where
  arbitrary = do
    a <- arbitrary
    EqPair . P a <$> arbitrary

instance Functor EqPair
  where
    fmap f (EqPair pair) = EqPair (mapPair f pair)


--Quad
newtype EqQuad a = EqQuad (Quad a)
  deriving (Show)

instance (Eq a) =>  Eq (EqQuad a)
  where
    EqQuad (Q a1 a2 a3 a4) == EqQuad (Q b1 b2 b3 b4) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4

instance Arbitrary a => Arbitrary (EqQuad a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    EqQuad . Q a1 a2 a3 <$> arbitrary

instance Functor EqQuad
  where
    fmap f (EqQuad quad) = EqQuad (mapQuad f quad)


--Annotated
newtype EqAnnotated e a = EqAnnotated (Annotated e a)
  deriving (Show)

instance (Eq a, Eq e) =>  Eq (EqAnnotated e a)
  where
    EqAnnotated (val1 :# ann1) == EqAnnotated (val2 :# ann2) = val1 == val2 && ann1 == ann2

instance (Arbitrary a, Arbitrary e) => Arbitrary (EqAnnotated e a) where
  arbitrary = do
    val <- arbitrary
    EqAnnotated . (:#) val <$> arbitrary

instance Functor (EqAnnotated e)
  where
    fmap f (EqAnnotated annotated) = EqAnnotated (mapAnnotated f annotated)

instance Monoid e => Applicative (EqAnnotated e) where
    pure a = EqAnnotated $ wrapAnnotated a
    (<*>) = undefined

instance Monoid e => Monad (EqAnnotated e) where
  (EqAnnotated m) >>= f = EqAnnotated $ joinAnnotated $ (\i -> let (EqAnnotated b) = f i in b) `mapAnnotated` m


--Except
newtype EqExcept e a = EqExcept (Except e a)
  deriving (Show)

instance (Eq a, Eq e) =>  Eq (EqExcept e a)
  where
    EqExcept (Error e1) == (EqExcept (Error e2))                   = e1 == e2
    EqExcept (HW3.T1.Success a1) == (EqExcept (HW3.T1.Success a2)) = a1 == a2
    _ == _                                                         = False

instance (Arbitrary a, Arbitrary e) => Arbitrary (EqExcept e a) where
  arbitrary = do
    n <- chooseInt (0, 1)
    case n of
      0 -> EqExcept . HW3.T1.Success <$> arbitrary
      _ -> EqExcept . Error <$> arbitrary

instance Functor (EqExcept e)
  where
    fmap f (EqExcept except) = EqExcept (mapExcept f except)

instance Applicative (EqExcept e) where
    pure a = EqExcept $ wrapExcept a
    (<*>) = undefined

instance Monad (EqExcept e) where
  (EqExcept m) >>= f = EqExcept $ joinExcept $ (\i -> let (EqExcept b) = f i in b) `mapExcept` m


--Prioritised
newtype EqPrioritised a = EqPrioritised (Prioritised a)
  deriving (Show)

instance (Eq a) =>  Eq (EqPrioritised a)
  where
    EqPrioritised (Low a) == EqPrioritised (Low b)       = a == b
    EqPrioritised (Medium a) == EqPrioritised (Medium b) = a == b
    EqPrioritised (High a) == EqPrioritised (High b)     = a == b
    _ == _                                               = False

instance Arbitrary a => Arbitrary (EqPrioritised a) where
  arbitrary = do
    n1 <- chooseInt (0, 1)
    n2 <- chooseInt (0, 1)
    case n1 + n2 of
      0 -> EqPrioritised . Low <$> arbitrary
      1 -> EqPrioritised . Medium <$> arbitrary
      _ -> EqPrioritised . High <$> arbitrary

instance Functor EqPrioritised
  where
    fmap f (EqPrioritised prior) = EqPrioritised (mapPrioritised f prior)


--Stream
newtype EqStream a = EqStream (Stream a)
  deriving (Show)

instance (Eq a) =>  Eq (EqStream a)
  where
    EqStream st1 == EqStream st2 = eqImpl (100::Int) st1 st2
      where
        eqImpl 0 _ _                     = True
        eqImpl num (a1 :> s1) (a2 :> s2) = a1 == a2 && eqImpl (num - 1) s1 s2

instance Arbitrary a => Arbitrary (EqStream a) where
  arbitrary = do
    a <- arbitrary
    st <- arbitrary
    return $ EqStream $ (:>) a (let (EqStream str) = st in str)

instance Functor EqStream
  where
    fmap f (EqStream st) = EqStream (mapStream f st)


--List
newtype EqList a = EqList (List a)
  deriving (Show)

instance (Eq a) =>  Eq (EqList a)
  where
    EqList (a :. l1) == EqList (b :. l2) = a == b && EqList l1 == EqList l2
    EqList Nil == EqList Nil             = True
    _ == _                               = False

instance Arbitrary a => Arbitrary (EqList a) where
  arbitrary = EqList . listToMyList <$> arbitrary
      where
        listToMyList (x : xs) = x :. listToMyList xs
        listToMyList _        = Nil

instance Applicative EqList where
    pure a = EqList $ wrapList a
    (<*>) = undefined

instance Monad EqList where
  (EqList m) >>= f = EqList $ joinList $ (\i -> let (EqList b) = f i in b) `mapList` m

instance Functor EqList
  where
    fmap f (EqList list) = EqList (mapList f list)




--------------------------------------------------
--LAWS
newtype FI = FI (Int -> Int)

instance Show FI
  where
    show _ = ""

instance Eq FI
  where
    (==) _ _ = True

instance Arbitrary FI
  where
    arbitrary = do
     let count = 1000
     list <- vectorOf count arbitrary
     return $ FI $ \i -> list !! abs i `mod` count

--T1
testFunctorLaws :: (Functor f, Eq (f Int)) => f Int -> FI -> FI -> Bool
testFunctorLaws functor f g = testIdentityLaw functor && testCompLaw functor f g

testIdentityLaw :: (Functor f, Eq (f Int)) => f Int -> Bool
testIdentityLaw functor = fmap id functor == functor

testCompLaw :: (Functor f, Eq (f Int)) => f Int -> FI -> FI -> Bool
testCompLaw functor (FI f) (FI g) = fmap (f . g) functor == (fmap f . fmap g) functor

--T2
testHomomorphismDistLaw :: forall f t. (Eq (t (Int, Int))) =>
 (forall a. f a -> t a) -> (forall a b. (f a, f b) -> f (a, b)) -> (forall a. a -> f a) -> Int -> Int -> Bool
testHomomorphismDistLaw constructor df wf a b =
  constructor (df (wf a, wf b)) == constructor (wf (a, b))

--T3
-- laws of join equivalents to laws of monad
testMonadLaws :: (Eq (m Int), Monad m) => m Int -> (Int -> m Int) -> Int -> FI -> FI -> Bool
testMonadLaws m constr val f g =
  testAssociativityLaw m f g
  && testLeftIdentityLaw val f constr
  && testRightIdentityLaw m


testLeftIdentityLaw :: (Eq (m Int), Monad m) => Int -> FI -> (Int -> m Int) -> Bool
testLeftIdentityLaw a (FI f) constructor = (return a >>= f1) == f1 a
  where
    f1 = constructor . f

testRightIdentityLaw :: (Eq (m b), Monad m) => m b -> Bool
testRightIdentityLaw monad = (monad >>= return) == monad

testAssociativityLaw :: (Eq (m Int), Monad m) => m Int -> FI -> FI -> Bool
testAssociativityLaw m (FI f) (FI g) = ((m >>= f1) >>= g1) == (m >>= (\x -> f1 x >>= g1))
  where
    f1 = pure . f
    g1 = pure . g

main :: IO ()
main = hspec $ do
  describe "T1 : functor laws tests" $ do
    it "Option" $
      property $ \functor ->
        testFunctorLaws (functor::(EqOption Int))
    it "Pair" $
      property $ \functor ->
        testFunctorLaws (functor::(EqPair Int))
    it "Quad" $
      property $ \functor ->
        testFunctorLaws (functor::(EqQuad Int))
    it "Annotated" $
      property $ \functor ->
        testFunctorLaws (functor::(EqAnnotated Int Int))
    it "Except" $
      property $ \functor ->
        testFunctorLaws (functor::(EqExcept Int Int))
    it "Prioritised" $
      property $ \functor ->
        testFunctorLaws (functor::(EqPrioritised Int))
    it "Stream" $
      property $ \functor ->
        testFunctorLaws (functor::(EqStream Int))
    it "List" $
      property $ \functor ->
        testFunctorLaws (functor::(EqList Int))
  describe "T2" $ do
    it "Option" $
      property $
        testHomomorphismDistLaw EqOption distOption wrapOption
    it "Pair" $
      property $
        testHomomorphismDistLaw EqPair distPair wrapPair
    it "Quad" $
      property $
        testHomomorphismDistLaw EqQuad distQuad wrapQuad
    it "Annotated" $
      property $
        testHomomorphismDistLaw (EqAnnotated::(Annotated String a -> EqAnnotated String a)) distAnnotated wrapAnnotated
    it "Except" $
      property $
        testHomomorphismDistLaw (EqExcept::(Except String a -> EqExcept String a)) distExcept wrapExcept
    it "Stream" $
      property $
        testHomomorphismDistLaw EqStream distStream wrapStream
    it "List" $
      property $
       testHomomorphismDistLaw EqList distList wrapList
  describe "T3 : monad laws tests" $ do
    it "Option" $
      property $ \monad ->
        testMonadLaws (monad::(EqOption Int)) pure
    it "Except" $
      property $ \monad ->
        testMonadLaws (monad::(EqExcept Int Int)) pure
    it "Annotated" $
      property $ \monad ->
        testMonadLaws (monad::(EqAnnotated String Int)) pure
    it "List" $
      property $ \monad ->
        testMonadLaws (monad::(EqList Int)) pure
