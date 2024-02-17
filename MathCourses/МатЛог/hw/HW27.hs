import Data.Either ( isLeft, isRight, lefts, rights) 


fa:: a -> b -> a
fa a b = a

fb :: (a, b) -> Either a b
fb (a, b) = Left a


fc :: (a, Either b c) -> Either (a, b) (a, c)
fc (a, x)  
    | isLeft x  = Left (a, head (lefts [x]))
    | isRight x =  Right (a, head (rights [x]))



fe :: (Either b c -> a) -> (b -> a, c -> a)
fe g = (g . Left , g . Right )


-- neg :: a -> b
-- neg = undefined 

ff :: (a -> b) -> (b -> z) -> (a -> z)
ff g p = p . g


fg :: ((a -> b) -> c) -> a -> b -> c
fg g x y = g (\a -> y)


fh :: (a -> b, a -> (b -> z)) -> a -> z
fh (g1, g2) a = g2 a (g1 a)

fia1 :: Either a b -> (a -> z, b -> z) -> z
fia1 x (g1, g2)
    | isLeft x  = g1 (head (lefts [x]))
    | otherwise = g2 (head (rights [x]))



fib1 :: (a -> z, b -> z) -> Either a b -> z
fib1 (g1, g2) x
    | isLeft x  = g1 (head (lefts [x]))
    | otherwise = g2 (head (rights [x]))


fib2 :: (Either a b -> z) -> (a -> z, b -> z)
fib2 g = (g . Left, g . Right)

-- z == b
fic2 :: Either (a -> b) b -> a -> b
fic2 x a
    | isLeft x  = head (lefts [x]) a
    | otherwise = head (rights [x])


fj :: a
fj = undefined




main:: IO ()
main = print ()

