sumMultiplesOf3n5 sealing =
    sum [ x | x <- [1..(sealing-1)], x `mod` 3 == 0 || x `mod` 5 == 0]

numberOfMultiples sealing x = (sealing - 1) / x

sumSerie n = (n * (n + 1)) / 2

sumMultiplesOf3n5' sealing =
   3 * sumSerie (fromIntegral (floor (numberOfMultiples sealing 3))) +
   5 * sumSerie (fromIntegral (floor (numberOfMultiples sealing 5))) -
   (3*5) * sumSerie (fromIntegral (floor (numberOfMultiples sealing 15)))