
h :: (a -> b, a -> (b -> z)) -> a -> z
h (g1, g2) a = g2 a (g1 a)





main:: IO ()
main = print ()


