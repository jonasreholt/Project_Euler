sumMultiplesOf3n5 sealing =
    sum [ x | x <- [1..(sealing-1)], x `mod` 3 == 0 || x `mod` 5 == 0]